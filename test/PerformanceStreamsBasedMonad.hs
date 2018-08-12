{-# LANGUAGE RankNTypes, ExplicitForAll, OverloadedStrings #-}
module PerformanceStreamsBasedMonad where

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit

import SBFMPerfBenchmark

import Control.Monad.State

validateResults :: [([Float], [Float])] -> Assertion
validateResults = mapM_ $ uncurry $ assertEqual "wrong result"

testSuite :: Test.Framework.Test
testSuite =
    testGroup
        "Performance SBFM"
        [
          -- testCase "4-step pipeline with Chans" $ validateResults $ coresTest runChanM
        -- ,
          -- testCase "4-step pipeline with par" $ validateResults $ coresTest runParIO
        -- ,
          testCase "4-step pipeline with PinnedChans" $ validateResults =<< (coresTest [1..4] $ flip evalStateT (0::Int))
        ]
