{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Data.Statistics
    ( OpCycle(..)
    , CycleStart
    , ReadState
    , RanUserFunction
    , WroteState
    , Next
    , initCycle
    , initNoOpCycle
    , mark
    , StatCollector
    , initStatCollection
    , recordCycle
    , Stats
    , getStats
    ) where

import Data.Word
import Data.Aeson.TH
import Ohua.Types
import Ohua.Serialize.JSON ()
import Data.IORef
import Control.Monad.IO.Class
import Data.Foldable

import qualified Data.Map.Strict as M

import qualified Foundation.Time.StopWatch as F
import qualified Foundation.Time.Types as F

data OpCycle = OpCycle
  { readingState :: !Word64
  , runningUserFunction :: !Word64
  , writingState :: !Word64
  , sendingResult :: !Word64
  , totalCycleTime :: !Word64
  }

deriveJSON defaultOptions ''OpCycle

newtype WroteState = WroteState (IO OpCycle)
newtype RanUserFunction = RanUserFunction (IO WroteState)
newtype ReadState = ReadState (IO RanUserFunction)
newtype CycleStart = CycleStart (IO ReadState)

initCycle :: MonadIO m => m CycleStart
initCycle = liftIO $ do
  clock <- F.startPrecise
  pure $ CycleStart $ do
    F.NanoSeconds readState <- F.stopPrecise clock
    pure $ ReadState $ do
      F.NanoSeconds ranUF <- F.stopPrecise clock
      pure $ RanUserFunction $ do
        F.NanoSeconds wroteState <- F.stopPrecise clock
        pure $ WroteState $ do
          F.NanoSeconds sentResult <- F.stopPrecise clock
          pure
            OpCycle
            { readingState = readState
            , runningUserFunction = ranUF - readState
            , writingState = wroteState - ranUF
            , sendingResult = sentResult - wroteState
            , totalCycleTime = sentResult
            }

initNoOpCycle :: MonadIO m => m CycleStart
initNoOpCycle =
    liftIO $
    pure $
    CycleStart $
    pure $
    ReadState $
    pure $ RanUserFunction $ pure $ WroteState $ pure $ OpCycle 0 0 0 0 0

type family Next stage where
  Next CycleStart = ReadState
  Next ReadState = RanUserFunction
  Next RanUserFunction = WroteState
  Next WroteState = OpCycle

class CycleMark stage where
  markIO :: stage -> IO (Next stage)

instance CycleMark CycleStart where
  markIO (CycleStart a) = a
instance CycleMark ReadState where
  markIO (ReadState a) = a
instance CycleMark RanUserFunction where
  markIO (RanUserFunction a) = a
instance CycleMark WroteState where
  markIO (WroteState a) = a

mark :: (CycleMark mark, MonadIO m) => mark -> m (Next mark)
mark = liftIO . markIO

newtype StatCollector = StatCollector (IORef [(QualifiedBinding, OpCycle)])

initStatCollection :: MonadIO m => m StatCollector
initStatCollection = liftIO $ StatCollector <$> newIORef mempty

recordCycle :: MonadIO m => StatCollector -> QualifiedBinding -> OpCycle -> m ()
recordCycle (StatCollector r) b cy = liftIO $ atomicModifyIORef' r $ \l -> ((b, cy):l,())

data OpStat = OpStat
  { operatorName :: QualifiedBinding
  , runs :: [OpCycle]
  , totalRuns :: Int
  , totalRuntime :: Integer
  , avgRun :: OpCycle
  }

type Stats = [OpStat]

getStats :: MonadIO m => StatCollector -> m Stats
getStats (StatCollector r) =
    liftIO $ do
        l <- readIORef r
        let opMap =
                foldr' (\(k, r) -> M.alter (Just . maybe [r] (r :)) k) mempty l
        pure $ map (uncurry toStat) $ M.toList opMap

toStat :: QualifiedBinding -> [OpCycle] -> OpStat
toStat opName runs =
    OpStat
        { operatorName = opName
        , runs = runs
        , totalRuns = length runs
        , totalRuntime = sum $ map (toInteger . totalCycleTime) runs
        , avgRun =
              OpCycle
                  { readingState = calcAvg readingState
                  , runningUserFunction = calcAvg runningUserFunction
                  , writingState = calcAvg writingState
                  , totalCycleTime = calcAvg totalCycleTime
                  , sendingResult = calcAvg sendingResult
                  }
        }
  where
    calcAvg f = fromInteger (sum (map (toInteger . f) runs) `div` numRuns)
    numRuns = toInteger $ length runs
