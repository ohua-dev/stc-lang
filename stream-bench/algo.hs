{-# LANGUAGE LambdaCase, ConstraintKinds, TupleSections, TypeApplications,
  OverloadedStrings, PartialTypeSignatures, FlexibleContexts, DeriveGeneric,
  TypeSynonymInstances, OverloadedLists, TypeFamilies #-}

import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData)
import Control.Exception (assert, bracket)
import Control.Monad ((<=<), (>=>), join, unless, void, forever)
import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState, get, gets, modify, put)
import Data.Aeson ((.:), decode)
import qualified Data.Aeson as AE
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Hashable
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as Tx
import Data.Time.Clock (getCurrentTime)
import Data.Typeable (Typeable)
import qualified Data.UUID.Types as UUID
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Aeson
import Monad.FuturesBasedMonad
import qualified MutableNFMap as NFMap
import qualified MutableSet as Set
import Prelude hiding (String, show)
import qualified Prelude as P
import System.Random (randomIO)
import System.Environment
import qualified Debug.Trace as Debug
import GHC.Exts (IsList, Item)
import Data.String (IsString)

import qualified Database.Redis as Redis

import qualified Kafka.Consumer as K

type String = BS.ByteString

type Message = BS.ByteString

type Long = Int64

data Window = Window
    { seenCount :: IORef Long
    , timestamp :: Text
    } deriving (Eq, Generic)

instance Hashable Window where
    hashWithSalt s w = hashWithSalt s (timestamp w)

instance NFData Window

instance AE.FromJSON String where
    parseJSON = AE.withText "Expected String" $ pure . Tx.encodeUtf8

fromRight :: Show a => Either a b -> b
fromRight = either (error . P.show) id

show :: Show a => a -> Text
show = Tx.pack . P.show

timeDivisor :: Long
timeDivisor = 10000

timeout :: Int
timeout = 1000 * 1000

-- NOTE I guessed this number. I have not seen any timeout specification in the
-- benchmark. This is in milliseconds.
kafkaTimeout :: K.Timeout
kafkaTimeout = K.Timeout 400000

-- NOTE The client also supports a `pollMessageBatch` function with returns
-- multiple messages at once, so we might want to implement the runtime or
-- algorithm to be able to make use of this if we are too slow
readKafka :: K.KafkaConsumer -> IO Message
readKafka con = do
    putStrLn "Reading from kafka... "
    subsState <- fromRight <$> K.subscription con
    putStrLn $ "Subscription state: \n" <> P.show subsState
    record <- fromRight <$> K.pollMessage con kafkaTimeout
    assert (isNothing $ K.crKey record) (pure ()) -- I just put this in here for
                                                 -- now so that we can reason
                                                 -- better about the structure
                                                 -- of the kafka message
    putStrLn "done"
    case K.crValue record of
        Nothing -> error "Empty response from Kafka"
        Just msg -> pure msg

getIndex :: Int -> [a] -> Maybe a
getIndex n _
    | n < 0 = Nothing
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
                checkR_ $ Redis.hset
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
                            checkR_ $ Redis.hset campaign "windows" rand
                            pure rand
                        Just uuid -> pure uuid
                checkR_ $ Redis.lpush windowListUUID [Tx.encodeUtf8 $ timestamp window]
                pure windowUUID
            Just uuid -> pure uuid
    checkR_ $ Redis.hincrby windowUUID "seen_count" . fromIntegral =<<
        liftIO (readIORef (seenCount window))
    liftIO $ writeIORef (seenCount window) 0
    time <- BS.pack . P.show <$> liftIO getCurrentTime
    checkR_ $ Redis.hset windowUUID "time_updated" time
    Redis.lpush "time_updated" [time]
  where
    -- NOTE This function is not necessary. It "only" forces the errors from the
    -- redis database. You can drop it and all its uses if you want to describe
    -- the algorithm. The reason I have it is so we notice if something goes
    -- wrong with the redis database.
    checkR_ :: (Monad f, Show err) => f (Either err a) -> f ()
    checkR_ = (either (error . P.show) (const $ pure ()) =<<)
    getUUID field =
        either (const Nothing) (join . getIndex 1) <$>
        Redis.hmget campaign [field]

redisGet :: MonadIO m => _ -> BS.ByteString -> m (Maybe BS.ByteString)
redisGet redisConn =
    liftIO . Redis.runRedis redisConn . fmap fromRight . Redis.get

isTheSame :: (Show a, Typeable a, Eq a, NFData a) => IO a -> STCLang a Bool
isTheSame init =
    liftWithState init $ \new -> do
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

-- | These are necessary because the Kafka client is an older version (0.8.2.1)
-- and does not support the `ApiVersionRequest` that the C-client library we use
-- under the hood sends in the beginning.
extraKafkaProperties :: (IsList l, Item l ~ (s0, s1), IsString s1, IsString s0) => l
extraKafkaProperties =
        [ ("api.version.request", "false")
        , ("broker.version.fallback", "0.8.2.1")
        ]

-- | Reads the config file at the specified path and creates the connection
-- objects we need
setup :: FilePath -> IO (K.KafkaConsumer, Redis.Connection)
setup loc = do
    conf <- fromRight <$> Yaml.decodeFileEither @AE.Value loc
    let topic = K.TopicName $ conf ^?! key "kafka.topic" . _String
    let sub = K.topics [topic]
        props =
            -- NOTE If I do not assign a group it fails immediately with
            -- "unknown group".
            K.groupId (K.ConsumerGroupId "ohua-stream-bench-group") <>
            K.extraProps extraKafkaProperties <>
            K.debugOptions [K.DebugAll] <>
            K.brokersList (
            map
                (\host ->
                     K.BrokerAddress $ host <> ":" <>
                     show (conf ^?! key "kafka.port" . _Integer))
                (conf ^.. key "kafka.brokers" . values . _String))
        rinfo =
            Redis.defaultConnectInfo
                { Redis.connectHost =
                      conf ^?! key "redis.host" . _String . to Tx.unpack
                }
    print topic
    print conf
    print $ K.cpProps props
    cons <- fmap fromRight (K.newConsumer props sub)
    (cons,) <$>
        Redis.checkedConnect rinfo

withInitial msg ac = do
    putStrLn $ "Doing initial " <> msg <> "..."
    r <- ac
    putStrLn $ msg <> " done"
    pure r

traceM :: Monad m => P.String -> m ()
traceM msg = Debug.trace msg $ pure ()

-- NOTE in the original implementation of the algorithm `rebalance` is the first
-- function called on the input stream. This distributes the messages
-- round-robin. This means we could also spawn multiple algorithm instances and
-- process multiple messages in parallel. But we'd have to ensure the timer
-- events are *not* round robin distributed!
algo kafkaConsumer redisConn
    -- Allocate the basic functions --
 = do
    traceM "Start allocations"
    let deserialize o =
            fromMaybe (error "Decoding error") $ do
                result <- decode $ LBS.fromStrict o
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
    traceM "Allocating redis"
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
    traceM "Allocating campaign"
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
                liftIO $ Redis.runRedis redisConn $
                    Set.mapM_
                        (\(cid, window) ->
                             Redis.runRedis redisConn $ writeRedis cid window)
                        s
            _ -> pure ()
    -- The condition we will use later for the if
    evCheck <- isTheSame getCurrentTime
    -- Allocate signals
    traceM "Allocating timer"
    timerSig <-
        liftSignal (threadDelay timeout >> getCurrentTime) (withInitial "time" getCurrentTime )
    -- Not sure if this is a good idea but I initialize here by polling the
    -- first message. Perhaps we should use a `Maybe` instead, however its not
    -- particularly convenient yet in our model so I do this.
    traceM "Allocating Kafka reader"
    msgSig <- liftSignal (readKafka kafkaConsumer) (withInitial "read kafka" $ readKafka kafkaConsumer)
    traceM "Allocating preprocessor"
    filteredProcessor
        -- NOTE keyBy partitions the operator state according to some key. If we
        -- have time we should implement that too
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

main, main0 :: IO ()
main = main0


main0 = do
    [confPath] <- getArgs
    void $ bracket
        (setup confPath)
        (\(kafkaConn, _redisConn) -> do
             print "Closing resources"
             K.closeConsumer kafkaConn
             -- Redis.disconnect redisConn
        )
        (\(kafkaConn, redisConn) -> putStrLn "Starting execution" >> runSignals (algo kafkaConn redisConn))
