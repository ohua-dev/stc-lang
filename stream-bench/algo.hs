{-# LANGUAGE LambdaCase, ConstraintKinds, TupleSections,
  TypeApplications, OverloadedStrings, PartialTypeSignatures #-}

import Prelude hiding (String)
import qualified CampaignProcMap as CPM
import Control.Monad ((<=<), (>=>), join, unless)
import qualified Data.HashTable.IO as MHT
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Void
import Monad.FuturesBasedMonad
import qualified MutableNFMap as NFMap
import qualified MutableSet as Set
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef')
import Control.Concurrent (threadDelay)
import Control.Monad.State.Class (get, gets, put, modify)
import Data.Bifunctor
import Control.Monad.IO.Class
import Data.Aeson ((.:), decode)
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData)
import Data.ByteString.Lazy (ByteString)
import Data.Hashable

type String = Text
type DeserializeS = Void
type Message = ByteString
type Long = Int64
data Window = Window
    { seenCount :: IORef Long
    , timestamp :: Text
    } deriving Eq

instance Hashable Window where
    hashWithSalt s w = hashWithSalt s (timestamp w)

timeDivisor :: Long
timeDivisor = 10000

timeout :: Int
timeout = 1000*000

readKafka :: IO Message
readKafka = undefined

writeRedis = undefined

redisGet :: MonadIO m => Text -> m (Maybe Text)
redisGet = undefined

isTheSame :: (Show a, Typeable a, Eq a, NFData a) => IO a -> STCLang a Bool
isTheSame init = liftWithState init $ \new -> do
    old <- get
    let isOld = old == new
    unless isOld $ put new
    pure $ old == new

redisJoinStateInit :: IO (NFMap.Map Text Text)
redisJoinStateInit = NFMap.new

getWindow :: _
getWindow = undefined

-- NOTE in the original implementation of the algorithm `rebalance` is the first
-- function called on the input stream. This distributes the messages
-- round-robin. This means we could also spawn multiple algorithm instances and
-- process multiple messages in parallel. But we'd have to ensure the timer
-- events are *not* round robin distributed!
algo
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
                        mcid <- redisGet adId
                        maybe
                            (return ())
                            (\cid -> NFMap.insert adId cid st)
                            mcid
                        pure mcid
    -- The campaign processor wrapped in the logic to separate handling of the
    -- timing event, regular and filtered data.
    processCampaign <-
        liftWithState ((,) <$> CPM.new <*> pure ()) $ \case
            Right (Just (campaignId, _adId, eventTime)) -> do
                flushCache <- gets fst
                let timeBucket = eventTime `div` timeDivisor
                window <- getWindow timeBucket campaignId
                liftIO $ modifyIORef' (seenCount window) (+ 1)
                let value = (campaignId, window)
                CPM.insert campaignId value flushCache
            Left timerTrigger -> do
                s <- gets fst
                newCache <- liftIO CPM.new
                modify (\(_, o) -> (newCache, o))
                CPM.mapM_ writeRedis s
            _ -> pure ()
    -- The condition we will use later for the if
    evCheck <- isTheSame getCurrentTime
    -- Allocate signals
    timerSig <-
        liftSignal (threadDelay timeout >> getCurrentTime) getCurrentTime
    msgSig <- liftSignal readKafka (pure undefined)
    filteredProcessor
        -- NOTE keyBy partitions the operator state according to some key.
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
