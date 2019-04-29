{-# LANGUAGE LambdaCase, ConstraintKinds, TupleSections, TypeApplications,
  OverloadedStrings, PartialTypeSignatures, FlexibleContexts, DeriveGeneric,
  TypeSynonymInstances #-}

import Prelude hiding (String, show)
import qualified Prelude as P
import qualified CampaignProcMap as CPM
import Control.Monad ((<=<), (>=>), join, unless)
import qualified Data.HashTable.IO as MHT
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as Tx
import Data.Time.Clock (getCurrentTime)
import Data.Void
import Monad.FuturesBasedMonad
import qualified MutableNFMap as NFMap
import qualified MutableSet as Set
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Control.Concurrent (threadDelay)
import Control.Monad.State.Class (MonadState, get, gets, put, modify)
import Data.Bifunctor
import Control.Monad.IO.Class
import Data.Aeson ((.:), decode)
import qualified Data.Aeson as AE
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (ByteString)
import Data.Hashable
import GHC.Generics
import qualified Database.Redis as Redis
import qualified Data.UUID.Types as UUID
import System.Random (randomIO)

type String = BS.ByteString
type DeserializeS = Void
type Message = ByteString
type Long = Int64
data Window = Window
    { seenCount :: IORef Long
    , timestamp :: Text
    } deriving (Eq, Generic)

instance Hashable Window where
    hashWithSalt s w = hashWithSalt s (timestamp w)

instance NFData Window where

instance AE.FromJSON String where
    parseJSON = AE.withText "Expected String" $ pure . Tx.encodeUtf8

show :: Show a => a -> Text
show = Tx.pack . P.show

timeDivisor :: Long
timeDivisor = 10000

timeout :: Int
timeout = 1000*000

readKafka :: IO Message
readKafka = undefined

getIndex :: Int -> [a] -> Maybe a
getIndex n _ | n < 0 = Nothing
getIndex _ [] = Nothing
getIndex 0 (x:_) = Just x
getIndex n (_:xs) = getIndex (n - 1) xs

writeRedis :: _ -> _ -> Redis.Redis _
writeRedis campaign window = do
    redisResponse <- getUUID (Tx.encodeUtf8 $ timestamp window)
    windowUUID <-
        case redisResponse of
            Nothing -> do
                windowUUID <-
                    LBS.toStrict . UUID.toByteString <$> liftIO randomIO
                Redis.hset
                    campaign
                    (Tx.encodeUtf8 $ timestamp window)
                    windowUUID
                redisResponse2 <- getUUID "windows"
                windowListUUID <-
                    case redisResponse2 of
                        Nothing -> do
                            rand <-
                                LBS.toStrict . UUID.toByteString <$>
                                liftIO randomIO
                            Redis.hset campaign "windows" rand
                            pure rand
                        Just uuid -> pure uuid
                Redis.lpush windowListUUID [Tx.encodeUtf8 $ timestamp window]
                pure windowUUID
            Just uuid -> pure uuid
    Redis.hincrby windowUUID "seen_count" . fromIntegral =<<
        liftIO (readIORef (seenCount window))
    liftIO $ writeIORef (seenCount window) 0
    time <- BS.pack . P.show <$> liftIO getCurrentTime
    Redis.hset windowUUID "time_updated" time
    Redis.lpush "time_updated" [time]
  where
    getUUID field =
        either (const Nothing) (join . getIndex 1) <$>
        Redis.hmget campaign [field]

redisGet :: MonadIO m => _ -> BS.ByteString -> m (Maybe BS.ByteString)
redisGet redisConn =
    liftIO .
    Redis.runRedis redisConn . fmap (either (error . P.show) id) . Redis.get

isTheSame :: (Show a, Typeable a, Eq a, NFData a) => IO a -> STCLang a Bool
isTheSame init = liftWithState init $ \new -> do
    old <- get
    let isOld = old == new
    unless isOld $ put new
    pure $ old == new

redisJoinStateInit :: IO (NFMap.Map _ _)
redisJoinStateInit = NFMap.new

newWindow :: MonadIO m => Long -> m Window
newWindow timeBucket =
    liftIO $ Window <$> newIORef 0 <*> pure (show $ timeBucket * timeDivisor)

-- NOTE This function doesn't do much. I did make an extra function for this
-- because I am not sure why they changed this function to be so simple, and why
-- they removed the call to redis that I assume was in here. I have a suspicion,
-- that the simplification here means that this benchmark is not as it was
-- described in the paper and because of that I leave the function in so we
-- remember to check it later.
redisGetWindow :: MonadIO m => Long -> m (Maybe Window)
redisGetWindow timeBucket = Just <$> newWindow timeBucket

-- NOTE As I describe below `keyBy` partitions the operator state along the
-- campaign id. For the flush set I use a hashmap on the campaign id to recreate
-- that behaviour. However for the windows there is already some splitting along
-- the campaign id, so I presume its okay not to split it again.
getWindow ::
       (MonadIO m, MonadState (w, NFMap.Map Long (NFMap.Map String Window)) m)
    => Long
    -> String
    -> m Window
getWindow timeBucket campaignId = do
    campaignWindows <- gets snd
    bucketMapE <-
        NFMap.lookup timeBucket campaignWindows >>= \case
            Just m -> pure $ Left m
            Nothing ->
                redisGetWindow timeBucket >>= \case
                    Nothing -> do
                        m <- NFMap.new
                        NFMap.insert timeBucket m campaignWindows
                        pure $ Left m
                    Just redisWindow -> do
                        m <- NFMap.new
                        NFMap.insert timeBucket m campaignWindows
                        pure $ Right redisWindow
    case bucketMapE of
        Right w -> pure w
        Left bucketMap ->
            NFMap.lookup campaignId bucketMap >>= \case
                Nothing -> do
                    window <-
                        maybe (newWindow timeBucket) pure =<<
                        redisGetWindow timeBucket
                    NFMap.insert campaignId window bucketMap
                    pure window
                Just window -> pure window

-- NOTE in the original implementation of the algorithm `rebalance` is the first
-- function called on the input stream. This distributes the messages
-- round-robin. This means we could also spawn multiple algorithm instances and
-- process multiple messages in parallel. But we'd have to ensure the timer
-- events are *not* round robin distributed!
algo redisConn
    -- Allocate the basic functions --
 = do
    let deserialize o =
            fromMaybe (error "Decoding error") $ do
                result <- decode o
                flip parseMaybe result $ \o ->
                    (,,,,,,) <$> (o .: "user_id" :: _ String) <*>
                    (o .: "page_id" :: _ String) <*>
                    (o .: "ad_id" :: _ String) <*>
                    (o .: "ad_type" :: _ String) <*>
                    (o .: "event_type" :: _ String) <*>
                    (o .: "event_time" :: _ Long) <*>
                    (o .: "ip_address" :: _ String)
    let evFilterFunc (_, _, _, _, t, _, _) = pure $ t == "view"
    let project (_, _, _, i3, _, i5, _) = pure (i3, i5)
    redisJoin <-
        liftWithState redisJoinStateInit $ \(adId, v2) ->
            fmap (, adId, v2) <$> do
                st <- get
                NFMap.lookup adId st >>= \case
                    Just cid -> pure $ Just cid
                    Nothing -> do
                        mcid <- redisGet redisConn adId
                        maybe
                            (return ())
                            (\cid -> NFMap.insert adId cid st)
                            mcid
                        pure mcid
    -- The campaign processor wrapped in the logic to separate handling of the
    -- timing event, regular and filtered data.
    processCampaign <-
        liftWithState ((,) <$> Set.new <*> NFMap.new) $ \case
            Right (Just (campaignId, _adId, eventTime)) -> do
                flushCache <- gets fst
                let timeBucket = eventTime `div` timeDivisor
                window <- getWindow timeBucket campaignId
                liftIO $ modifyIORef' (seenCount window) (+ 1)
                let value = (campaignId, window)
                Set.insert value flushCache
            Left timerTrigger -> do
                s <- gets fst
                newCache <- liftIO Set.new
                modify (\(_, o) -> (newCache, o))
                liftIO $
                    Redis.runRedis redisConn $
                    Set.mapM_
                        (\(cid, window) ->
                             Redis.runRedis redisConn $ writeRedis cid window)
                        s
            _ -> pure ()
    -- The condition we will use later for the if
    evCheck <- isTheSame getCurrentTime
    -- Allocate signals
    timerSig <-
        liftSignal (threadDelay timeout >> getCurrentTime) getCurrentTime
    msgSig <- liftSignal readKafka (pure undefined)
    filteredProcessor
        -- NOTE keyBy partitions the operator state according to some key. I did
        -- not immediately see a simple way to implement this in our sytem.
        -- That's why, for now I ignore keyBy and implement it instead using a
        -- HashMap in the processor function
         <-
        filterSignalM
            evFilterFunc
            (project >=> redisJoin
                                       -- >=> keyBy 0
             )
    -- The actual algorithm
    return $ \src -> do
        timerEv <- timerSig src
        msgEv <- msgSig src
        -- Fork on whether this is a timing event
        procInput <-
            if_
                (evCheck timerEv)
                (pure $ Left timerEv)
                (Right <$> do
                     msg <- pure $ deserialize msgEv
                     join <$> filteredProcessor msg)
        processCampaign procInput

main = return ()
