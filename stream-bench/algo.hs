{-# LANGUAGE LambdaCase, ConstraintKinds, TupleSections,
  TypeApplications, OverloadedStrings, PartialTypeSignatures,
  FlexibleContexts, DeriveGeneric, TypeSynonymInstances,
  OverloadedLists, TypeFamilies, MultiWayIf, RecordWildCards,
  NamedFieldPuns #-}

import Control.Concurrent (forkIO, killThread, newEmptyMVar, threadDelay)
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.BoundedChan as BC
import Control.Concurrent.MVar (putMVar, takeMVar)
import Control.DeepSeq (NFData, deepseq)
import Control.Exception (assert, bracket)
import Control.Monad
  ( (<=<)
  , (>=>)
  , forM
  , forM_
  , forever
  , join
  , replicateM
  , unless
  , void
  , when
  )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.List (ListT(ListT), runListT)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.SD
import Control.Monad.State.Class as S
  ( MonadState
  , get
  , gets
  , modify
  , put
  , state
  )
import Control.Monad.State.Lazy as S (StateT, runStateT)
import Data.Aeson ((.:), decode)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Hashable
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.StateElement
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as Tx
import qualified Data.Text.IO as Tx
import Data.Typeable (Typeable)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Word (Word32)
import qualified Data.Yaml as Yaml
import qualified Debug.Trace as Debug
import GHC.Exts (IsList, Item)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Aeson
import Monad.Generator (Generator, foldlGeneratorT, ioReaderToGenerator)
import qualified MutableNFMap as NFMap
import qualified MutableSet as Set
import Prelude hiding (String, show)
import qualified Prelude as P
import qualified System.Clock as Clock
import System.Environment
import System.IO (Handle, IOMode(WriteMode), withFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)
import System.Random (randomIO, randomRIO)
import Text.Printf (hPrintf, printf)

import qualified Database.Redis as Redis

import qualified Kafka.Consumer as K

type String = BS.ByteString

type Message = LBS.ByteString

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

data Collector a = Collector
  { counts :: IORef (UMV.IOVector a)
  , lastIndex :: IORef Int
  }

collectorInitialSize :: Int
collectorInitialSize = 400

newCollector :: (MonadIO m, UMV.Unbox a) => m (Collector a)
newCollector =
  liftIO $ Collector <$> (newIORef =<< UMV.unsafeNew collectorInitialSize) <*>
  newIORef 0

push :: (MonadIO m, UMV.Unbox a) => Collector a -> a -> m ()
push Collector {..} i =
  liftIO $ do
    indx <- readIORef lastIndex
    c <- readIORef counts
    let l = UMV.length c
    assert (indx <= l && indx > 0) (pure ())
    c' <-
      if l == indx
        then do
          new <- UMV.unsafeGrow c l
          writeIORef counts new
          pure new
        else pure c
    UMV.write c' indx i
    modifyIORef' lastIndex succ

unsafeRead :: (MonadIO m, UMV.Unbox a) => Collector a -> m (UV.Vector a)
unsafeRead Collector {..} =
  liftIO $ do
    idx <- readIORef lastIndex
    UV.unsafeFreeze . UMV.slice 0 idx =<< readIORef counts

data Statistics = Statistics
  { fetcherEventCount :: Collector Int
  , pipelineEventCount :: Collector Word32
  , redisWritesCount :: Collector Int
  }

initStatistics :: IO Statistics
initStatistics = Statistics <$> newCollector <*> newCollector <*> newCollector

writeStatistics :: MonadIO m => Statistics -> Handle -> m ()
writeStatistics Statistics {..} h =
  liftIO $ do
    hPrintf
      h
      "%15s %15s %15s\n"
      ("Fetcher Events" :: P.String)
      ("Pipeline Events" :: P.String)
      ("Write events" :: P.String)
    fec <- unsafeRead fetcherEventCount
    pec <- unsafeRead pipelineEventCount
    rwc <- unsafeRead redisWritesCount
    UV.forM_ (UV.zip3 fec pec rwc) $ \(fcount, pcount, rcount) ->
      hPrintf h "%-15d %-15d %-15d\n" fcount pcount rcount
    let l1 = UV.length fec
        l2 = UV.length pec
        l3 = UV.length rwc
    unless (l1 == l2 && l2 == l3) $
      printf "Lengths were unequal: fec=%7d pec=%7d rwc=%7d\n" l1 l2 l3

fromRight :: Show a => Either a b -> b
fromRight = either (error . P.show) id

show :: Show a => a -> Text
show = Tx.pack . P.show

timeDivisor :: Long
timeDivisor = 10 * 1000

-- | Timeout for the timer trigger. In the actual benchmarks this is one second.
-- I choose 10 seconds here because in one second only very little happens
timeout :: Int
timeout = 1000 * 1000 * 10

eventGenerationStep = 100 * 1000

kafkaEventCount = 10 * 1000 * 1000

numCampaigns = 100

currentMilliSecs :: IO Long
currentMilliSecs =
  (`div` (1000 * 1000)) . fromIntegral . Clock.toNanoSecs <$>
  Clock.getTime Clock.Realtime

-- | Do this for `kafkaEventCount` many times
generateKafkaEvents :: V.Vector Text -> IO [AE.Value]
generateKafkaEvents ads = do
  startTime <-
    fromIntegral . (* eventGenerationStep) . Clock.toNanoSecs <$>
    Clock.getTime Clock.Realtime
  runListT $ do
    n <- ListT $ pure [0 .. 10000 :: Word]
    userId <- liftIO $ randomIO @UUID.UUID
    pageId <- liftIO $ randomIO @UUID.UUID
    ad <- randomIn ads
    adType <- randomIn adTypes
    eventType <- randomIn eventTypes
    pure $
      AE.object
        [ "user_id" AE..= userId
        , "page_id" AE..= pageId
        , "ad_id" AE..= ad
        , "ad_type" AE..= adType
        , "event_type" AE..= eventType
        , "event_time" AE..= (startTime + (n * 10) + skew + lateBy)
        , "ip_address" AE..= ("1.2.3.4" :: Text)
        ]
  where
    adTypes =
      V.fromList
        ["banner", "modal", "sponsored-search", "mail", "mobile" :: Text]
    eventTypes = V.fromList ["view", "click", "purchase" :: Text]
    skew = 0
    lateBy = 0
    randomIn l = (l V.!) <$> liftIO (randomRIO (0, V.length l - 1))

eventGenerationLoop :: ([Message] -> IO ()) -> IO ()
eventGenerationLoop writer = do
  ads <-
    V.fromList <$> replicateM (10 * numCampaigns) (UUID.toText <$> randomIO)
    --ads <- Tx.lines <$> Tx.readFile "ad-ids.txt"
  forM_ @[] @IO @Word [0,fromIntegral eventGenerationStep .. kafkaEventCount] $ \_ -> do
    evs <- generateKafkaEvents ads
    let evaluated = map AE.encode evs
    evaluated `deepseq` pure ()
    putStrLn "Writing Events"
    writer evaluated

-- NOTE I guessed this number. I have not seen any timeout specification in the
-- benchmark. This is in milliseconds.
kafkaTimeout :: K.Timeout
kafkaTimeout = K.Timeout 4000

-- NOTE I also guessed this number. No idea whether this value is appropriate.
kafkaBatchSize :: K.BatchSize
kafkaBatchSize = K.BatchSize 400

readKafka :: K.KafkaConsumer -> Statistics -> IO [Message]
readKafka con stats = do
  subsState <- fromRight <$> K.subscription con
    --putStrLn $ "Subscription state: \n" <> P.show subsState
  resps <- K.pollMessageBatch con kafkaTimeout (K.BatchSize batchSize)
  oldCount <- readIORef msgsRead
  time <- currentMilliSecs
  oldTime <- readIORef timeFrame
  let msgs =
        map
          (LBS.fromStrict . fromJust . K.crValue .
           (\a -> assert (isNothing $ K.crKey a) a) .
           fromRight)
          resps
  let !newCount = oldCount + length msgs
  if (time - oldTime > 10000)
    then do
      writeIORef timeFrame time
      writeIORef msgsRead 0
      putStrLn $ "Fetched " <> P.show newCount <>
        " messages in the last ~10 seconds"
      push (fetcherEventCount stats) newCount
    else writeIORef msgsRead newCount
               -- I just put this assert in here for now so that we can reason
               -- better about the structure of the kafka message
    --msgs `deepseq` putStrLn "done"
  pure msgs
  where
    {-# NOINLINE msgsRead #-}
    {-# NOINLINE timeFrame #-}
    msgsRead = unsafePerformIO $ newIORef 0
    timeFrame = unsafePerformIO $ newIORef =<< currentMilliSecs
    batchSize = 400

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
        windowUUID <- encodeUUID <$> liftIO randomIO
        checkR_ $
          Redis.hset campaign (Tx.encodeUtf8 $ timestamp window) windowUUID
        redisResponse2 <- getUUID "windows"
        windowListUUID <-
          case redisResponse2 of
            Nothing -> do
              rand <- encodeUUID <$> liftIO randomIO
              checkR_ $ Redis.hset campaign "windows" rand
              pure rand
            Just uuid -> pure uuid
        checkR_ $ Redis.lpush windowListUUID [Tx.encodeUtf8 $ timestamp window]
        pure windowUUID
      Just uuid -> pure uuid
  checkR_ $ Redis.hincrby windowUUID "seen_count" . fromIntegral =<<
    liftIO (readIORef (seenCount window))
  liftIO $ writeIORef (seenCount window) 0
  time <- BS.pack . P.show <$> liftIO currentMilliSecs
  checkR_ $ Redis.hset windowUUID "time_updated" time
  Redis.lpush "time_updated" [time]
    -- NOTE This function is not necessary. It "only" forces the errors from the
    -- redis database. You can drop it and all its uses if you want to describe
    -- the algorithm. The reason I have it is so we notice if something goes
    -- wrong with the redis database.
  where
    checkR_ :: (Monad f, Show err) => f (Either err a) -> f ()
    checkR_ = (either (error . P.show) (const $ pure ()) =<<)
    getUUID field =
      either (const Nothing) (join . getIndex 0) <$>
      Redis.hmget campaign [field]
    encodeUUID = BS.pack . P.show :: UUID.UUID -> BS.ByteString

redisGet :: MonadIO m => _ -> BS.ByteString -> m (Maybe BS.ByteString)
redisGet redisConn =
  liftIO . Redis.runRedis redisConn . fmap fromRight . Redis.get

isTheSame :: (Show a, Typeable a, Eq a, NFData a) => IO a -> STCLang a Bool
isTheSame init =
  liftWithState init $ \new -> do
    old <- get
    let isOld = old == new
    unless isOld $ put new
    pure $ isOld

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
            maybe (newWindow timeBucket) pure =<< redisGetWindow timeBucket
          NFMap.insert campaignId window bucketMap
          pure window
        Just window -> pure window

-- | These are necessary because the Kafka client is an older version (0.8.2.1)
-- and does not support the `ApiVersionRequest` that the C-client library we use
-- under the hood sends in the beginning.
extraKafkaProperties ::
     (IsList l, Item l ~ (s0, s1), IsString s1, IsString s0) => l
extraKafkaProperties =
  [("api.version.request", "false"), ("broker.version.fallback", "0.8.2.1")]

type KafkaReader = IO LBS.ByteString

type CloseKafka = IO ()

type KafkaActions = (KafkaReader, CloseKafka)

cachedBackoffReader :: _ -> IO [a] -> IO (IO a)
cachedBackoffReader backoff refetch = do
  cache <- newIORef []
  let go =
        readIORef cache >>= \case
          [] -> do
            new <- refetch
            if null new
              then putStrLn "Refetch returned empty response" >>
                   threadDelay backoff
              else writeIORef cache new
            go
          (x:xs) -> writeIORef cache xs >> pure x
  pure go

setupMockKafka :: _ -> IO KafkaActions
setupMockKafka _conf = do
  kafkaVar <- newEmptyMVar
  let kafkaWriter m = putMVar kafkaVar m
  kafkaReader <- cachedBackoffReader 100 (takeMVar kafkaVar)
  writerThread <- forkIO $ eventGenerationLoop kafkaWriter
  pure (kafkaReader, killThread writerThread)

setupKafka :: Statistics -> AE.Value -> IO KafkaActions
setupKafka stats conf = do
  print topic
  print conf
  print $ K.cpProps props
  cons <- fromRight <$> (K.newConsumer props sub)
  reader <- cachedBackoffReader 100 $ readKafka cons stats
  pure (reader, maybe (pure ()) (error . P.show) =<< K.closeConsumer cons)
  where
    topic = K.TopicName $ conf ^?! key "kafka.topic" . _String
    sub = K.topics [topic] <> K.offsetReset K.Latest
    props
            -- NOTE If I do not assign a group it fails immediately with
            -- "unknown group".
     =
      K.groupId (K.ConsumerGroupId "ohua-stream-bench-group") <>
        --K.extraProps extraKafkaProperties <>
        --K.debugOptions [K.DebugAll] <>
      K.brokersList
        (map
           (\host ->
              K.BrokerAddress $ host <> ":" <>
              show (conf ^?! key "kafka.port" . _Integer))
           (conf ^.. key "kafka.brokers" . values . _String))

-- | Reads the config file at the specified path and creates the connection
-- objects we need
setup :: FilePath -> IO (KafkaActions, Redis.Connection, Statistics)
setup loc = do
  conf <- fromRight <$> Yaml.decodeFileEither @AE.Value loc
  let rinfo =
        Redis.defaultConnectInfo
          { Redis.connectHost =
              conf ^?! key "redis.host" . _String . to Tx.unpack
          }
    -- cons <- fmap fromRight (K.newConsumer props sub)
  stats <- initStatistics
  (,,) <$> setupKafka stats conf <*> Redis.checkedConnect rinfo <*> pure stats

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
algo kafkaReader redisConn stats
    -- Allocate the basic functions --
 = do
  traceM "Start allocations"
  let deserialize o =
        either (error . ("Decoding error: " <>)) id $ do
          result <- AE.eitherDecode o
          flip AE.parseEither result $ \o ->
            (,,,,,,) <$> (o .: "user_id" :: _ String) <*>
            (o .: "page_id" :: _ String) <*>
            (o .: "ad_id" :: _ String) <*>
            (o .: "ad_type" :: _ String) <*>
            (o .: "event_type" :: _ String) <*>
            (read <$> o .: "event_time") <*>
            (o .: "ip_address" :: _ String)
  let evFilterFunc ~(_, _, _, _, t, _, _) = pure $ t == "view"
  let project ~(_, _, i2, _, _, i5, _) = pure (i2, i5)
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
              (liftIO $ putStrLn "Ad campaign not found in redis")
              (\cid -> NFMap.insert adId cid st)
              mcid
            pure mcid
    -- The campaign processor wrapped in the logic to separate handling of the
    -- timing event, regular and filtered data.
  emitCounter <-
    liftWithState (pure 0 :: _ Word32) $ \case
      Right _ -> modify succ
      Left _ -> do
        c <- get
        put 0
        liftIO $ putStrLn $ "Saw " <> P.show c <> " events total in time window"
        push (pipelineEventCount stats) c
  traceM "Allocating campaign"
  processCampaign <-
    liftWithState ((,) <$> Set.new <*> NFMap.new) $ \case
      Right (Just ev@(campaignId, _adId, eventTime)) -> do
        flushCache <- gets fst
        let timeBucket = eventTime `div` timeDivisor
        window <- getWindow timeBucket campaignId
        liftIO $ modifyIORef' (seenCount window) (+ 1)
        let value = (campaignId, window)
        Set.insert value flushCache
      Left timerTrigger -> do
        (s, cache) <- get
        newCache <- Set.new
        modify (\(_, o) -> (newCache, o))
        asList <- Set.toList s
                --liftIO $ printf "I touched %d campaigns" (length $ nub $ map fst asList)
        let l = length asList
        push (redisWritesCount stats) l
        liftIO $ do
          putStrLn $ "Initiated redis write of " <> P.show l <> " events"
          Redis.runRedis redisConn $
            mapM_
              (\(cid, window) -> do
                 c <- liftIO $ readIORef $ seenCount window
                                   --liftIO $ printf "Writing count %4d for timestamp %v\n" c (timestamp window)
                 writeRedis cid window)
              asList
      _ -> pure ()
    -- The condition we will use later for the if
  evCheck <- isTheSame currentMilliSecs
    -- Allocate signals
  traceM "Allocating timer"
  timerSig <-
    liftSignal
      (threadDelay timeout >> currentMilliSecs)
      (withInitial "time" currentMilliSecs)
    -- Not sure if this is a good idea but I initialize here by polling the
    -- first message. Perhaps we should use a `Maybe` instead, however its not
    -- particularly convenient yet in our model so I do this.
  traceM "Allocating Kafka reader"
  msgSig <- liftSignal kafkaReader (withInitial "read kafka" kafkaReader)
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
        (Right <$> do
           msg <- pure $ deserialize msgEv
           join <$> filteredProcessor msg)
        (pure $ Left timerEv)
    emitCounter procInput
    processCampaign procInput

data CollSt = CollSt
  { states :: [S]
  , signals :: [IO S]
  }

instance Monoid CollSt where
  mempty = CollSt [] []
  CollSt st1 si1 `mappend` CollSt st2 si2 =
    CollSt (st1 `mappend` st2) (si1 `mappend` si2)

algoSeq kafkaReader redisConn stats
    -- Allocate the basic functions --
 =
  runSignals $ do
    traceM "Start allocations"
    let deserialize o =
          either (error . ("Decoding error: " <>)) id $ do
            result <- AE.eitherDecode o
            flip AE.parseEither result $ \o ->
              (,,,,,,) <$> (o .: "user_id" :: _ String) <*>
              (o .: "page_id" :: _ String) <*>
              (o .: "ad_id" :: _ String) <*>
              (o .: "ad_type" :: _ String) <*>
              (o .: "event_type" :: _ String) <*>
              (read <$> o .: "event_time") <*>
              (o .: "ip_address" :: _ String)
    let evFilterFunc ~(_, _, _, _, t, _, _) = pure $ t == "view"
    let project ~(_, _, i2, _, _, i5, _) = pure (i2, i5)
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
                (liftIO $ putStrLn "Ad campaign not found in redis")
                (\cid -> NFMap.insert adId cid st)
                mcid
              pure mcid
    -- The campaign processor wrapped in the logic to separate handling of the
    -- timing event, regular and filtered data.
    emitCounter <-
      liftWithState (pure 0 :: _ Word32) $ \case
        Right _ -> modify succ
        Left _ -> do
          c <- get
          put 0
          liftIO $ putStrLn $ "Saw " <> P.show c <>
            " events total in time window"
          push (pipelineEventCount stats) c
    traceM "Allocating campaign"
    processCampaign <-
      liftWithState ((,) <$> Set.new <*> NFMap.new) $ \case
        Right (Just ev@(campaignId, _adId, eventTime)) -> do
          flushCache <- gets fst
          let timeBucket = eventTime `div` timeDivisor
          window <- getWindow timeBucket campaignId
          liftIO $ modifyIORef' (seenCount window) (+ 1)
          let value = (campaignId, window)
          Set.insert value flushCache
        Left timerTrigger -> do
          (s, cache) <- get
          newCache <- Set.new
          modify (\(_, o) -> (newCache, o))
          asList <- Set.toList s
                --liftIO $ printf "I touched %d campaigns" (length $ nub $ map fst asList)
          let l = length asList
          push (redisWritesCount stats) l
          liftIO $ do
            putStrLn $ "Initiated redis write of " <> P.show l <> " events"
            Redis.runRedis redisConn $
              mapM_
                (\(cid, window) -> do
                   c <- liftIO $ readIORef $ seenCount window
                                   --liftIO $ printf "Writing count %4d for timestamp %v\n" c (timestamp window)
                   writeRedis cid window)
                asList
        _ -> pure ()
    -- The condition we will use later for the if
    evCheck <- isTheSame currentMilliSecs
    -- Allocate signals
    traceM "Allocating timer"
    timerSig <-
      liftSignal
        (threadDelay timeout >> currentMilliSecs)
        (withInitial "time" currentMilliSecs)
    -- Not sure if this is a good idea but I initialize here by polling the
    -- first message. Perhaps we should use a `Maybe` instead, however its not
    -- particularly convenient yet in our model so I do this.
    traceM "Allocating Kafka reader"
    msgSig <- liftSignal kafkaReader (withInitial "read kafka" kafkaReader)
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
    pure $ \src -> do
      timerEv <- timerSig src
      msgEv <- msgSig src
        -- Fork on whether this is a timing event
      procInput <-
        if_
          (evCheck timerEv)
          (Right <$> do
             msg <- pure $ deserialize msgEv
             join <$> filteredProcessor msg)
          (pure $ Left timerEv)
      emitCounter procInput
      processCampaign procInput
  where
    isTheSame init =
      liftWithState init $ \new -> do
        old <- get
        let isOld = old == new
        unless isOld $ put new
        pure $ isOld
    printSignalD = putStrLn
    runOhuaM comp states = do
      v <- V.thaw (V.fromList states)
      a <- runReaderT comp v
      (a, ) . V.toList <$> V.freeze v
    if_ c t e = do
      c' <- c
      if c'
        then t
        else e
    filterSignalM cond f =
      pure $ \item -> if_ (cond item) (Just <$> f item) (pure Nothing)
    liftWithState ::
         (Typeable s, NFData s)
      => IO s
      -> (SF s a b)
      -> BuildSeqOhua (a -> SeqOhua b)
    liftWithState state stateThread = do
      s0 <- lift state
      l <-
        S.state $ \s -> (length $ states s, s {states = states s ++ [toS s0]})
      pure $ \a -> do
        v <- ask
        liftIO $ do
          s <- fromS <$> MV.read v l
          (b, s') <- runStateT (stateThread a) s
          MV.write v l $ toS s'
          pure b
    liftSignal ::
         (Typeable a, NFData a)
      => IO a
      -> IO a
      -> BuildSeqOhua (Signals -> SeqOhua a)
    liftSignal s0 init = do
      idx <-
        S.state $ \s@CollSt {signals} ->
          (length signals, s {signals = signals ++ [toS <$> s0]})
      liftWithState init $ \(i, s) ->
        if i == idx
          then do
            let my = fromS s
            S.put my
            pure my
          else S.get
    runSignals comp = do
      (comp', s) <- S.runStateT comp mempty
      chan <- BC.newBoundedChan 100
      bracket
        (do forM (zip [0 ..] $ signals s) $ \(idx, sig) ->
              Conc.forkIO $ forever $ do
                event <- sig
                BC.writeChan chan $ Just (idx, event))
        (\threads -> do
           printSignalD "Killing signal threads"
           mapM_ Conc.killThread threads)
        (\_ -> do
           printSignalD "signals done"
           let signalGen =
                 ioReaderToGenerator @(Generator IO) (BC.readChan chan)
           runOhuaM (smapGen comp' signalGen) $ states s)
    smapGen f = foldlGeneratorT lift (\acc i -> (: acc) <$> f i) []

type BuildSeqOhua a = StateT CollSt IO a

type SeqOhua a = ReaderT (MV.IOVector S) IO a

main, main0 :: IO ()
main = main0

main0 = do
  [confPath] <- getArgs
  runSequential <- maybe False (/= "") <$> lookupEnv "SEQUENTIAL"
  void $
    bracket
      (setup confPath)
      (\((_, closeKafka), _redisConn, stats) -> do
         putStrLn "Closing resources"
         closeKafka
         putStrLn "Writing Statistics"
         withFile "ohua-statistics.txt" WriteMode $ \h ->
           writeStatistics stats h
             -- Redis.disconnect redisConn
       )
      (\((kafkaReader, _), redisConn, stats) ->
         putStrLn "Starting execution" >>
         if runSequential
           then algoSeq kafkaReader redisConn stats
           else runSignals (algo kafkaReader redisConn stats))
