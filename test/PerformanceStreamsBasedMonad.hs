{-# LANGUAGE RankNTypes, ExplicitForAll #-}
module PerformanceStreamsBasedMonad where

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit

import FakeComputation (work,wrk_sins)

import Monad.StreamsBasedExplicitAPI
import Monad.StreamsBasedFreeMonad
import Data.Dynamic2

import Data.Time.Clock.POSIX

import Control.Monad.Stream.Par
import Control.Monad.Stream.Chan
import Control.Monad.Stream.PinnedChan
import Control.Monad.Stream

import Control.Monad.State

import GHC.Conc (numCapabilities,setNumCapabilities,getNumCapabilities)


currentTimeMillis = round . (* 1000) <$> getPOSIXTime

pipeline v = do
  c <- return v
  r0 <- liftWithIndex 0 work c
  r1 <- liftWithIndex 1 (work . snd) r0
  r2 <- liftWithIndex 2 (work . snd) r1
  r3 <- liftWithIndex 3 (work . snd) r2
  r  <- liftWithIndex 4 snd' r3
  return r

snd' :: (a,b) -> StateT () IO b
snd' = return . snd

fourStepPipeline = smap pipeline

-- Beware: You need to recompile with "-threaded" in order to  enable concurrency!
--         Just changing the cabal file and running `stack test` won't work.
--         Instead always do `stack clean && stack test`
pipeSMapTest :: MonadStream m => (forall a . m a -> IO a) -> Assertion
pipeSMapTest run = do
  let a = 3000000 :: Float
  let b = 2000000 :: Int
  let inputs = replicate 4 a
  let r = wrk_sins b a
  let expectedOutputs = replicate 4 r
  putStrLn $ "num cores (RTS option): " ++ (show numCapabilities)
  (\x -> putStrLn $ "num cores: " ++ show x) =<< getNumCapabilities
  start <- currentTimeMillis
  result <- run $ runOhuaM (fourStepPipeline =<< sfConst' inputs) $ map toDyn [(0::Int,b),(1,b),(2,b),(3,b),undefined]
  stop <- currentTimeMillis

  putStrLn $ "Exec time [ms]: " ++ (show $ stop - start)
  assertEqual "result was wrong." expectedOutputs result

coresTest :: MonadStream m => (forall a . m a -> IO a) -> Assertion
coresTest runner = mapM_ runTest [1..4]
  where
    runTest numCores = do
      setNumCapabilities numCores
      pipeSMapTest runner

      -- TODO validation needed! (for now, check the exec times)

testSuite :: Test.Framework.Test
testSuite =
    testGroup
        "Performance SBFM"
        [
          -- testCase "4-step pipeline with Chans" $ coresTest runChanM
        -- ,
          -- testCase "4-step pipeline with par" $ coresTest runParIO
        -- ,
          testCase "4-step pipeline with Chans" $ coresTest $ flip evalStateT (0::Int)
        ]
