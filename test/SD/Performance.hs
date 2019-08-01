module SD.Performance (testSuite) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (State)

import FakeComputation (work, wrk_sins)

import Control.Monad.SD
import Data.StateElement

import Data.Time.Clock.POSIX

import GHC.Conc (getNumCapabilities, numCapabilities, setNumCapabilities)

currentTimeMillis = round . (* 1000) <$> getPOSIXTime

pipeline v = do
    c <- return v
    (_, r0) <- liftWithIndex 0 work c
    (_, r1) <- liftWithIndex 1 work r0
    (_, r2) <- liftWithIndex 2 work r1
    (_, r3) <- liftWithIndex 3 work r2
    return r3

fourStepPipeline = smap pipeline

-- Beware: You need to recompile with "-threaded" in order to  enable concurrency!
--         Just changing the cabal file and running `stack test` won't work.
--         Instead always do `stack clean && stack test`
pipeSMapTest :: Assertion
pipeSMapTest = do
    let a = 3000000 :: Float
    let b = 2000000 :: Int
    let inputs = replicate 4 a
    let r = wrk_sins b a
    let expectedOutputs = replicate 4 r
    putStrLn $ "num cores (RTS option): " ++ (show numCapabilities)
    (\x -> putStrLn $ "num cores: " ++ show x) =<< getNumCapabilities
    start <- currentTimeMillis
    (result, _) <-
        runOhuaM (fourStepPipeline inputs) $
        map toS $ [(0 :: Int, b), (1, b), (2, b), (3, b)]
    stop <- currentTimeMillis
    putStrLn $ "Exec time [ms]: " ++ (show $ stop - start)
    assertEqual "result was wrong." expectedOutputs result

coresTest = mapM_ runTest [1 .. 4]
  where
    runTest numCores = do
        setNumCapabilities numCores
        pipeSMapTest
      -- TODO validation needed! (for now, check the exec times)

testSuite :: Test.Framework.Test
testSuite = testGroup "Performance FBM" [testCase "4-step pipeline" coresTest]
