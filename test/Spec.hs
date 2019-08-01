{-# LANGUAGE OverloadedLists #-}
import Test.Framework

import SD.Correctness as FBM
import SD.Performance as PFBM


main :: IO ()
main =
    defaultMain
        [FBM.testSuite, PFBM.testSuite]
-- main = flip defaultMainWithOpts mempty FBM.testSuite
