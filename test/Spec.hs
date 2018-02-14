
import Test.Framework

import CorrectnessFuturesBasedMonad as FBM
import CorrectnessStreamsBasedMonad as SBM

main :: IO ()
main = flip defaultMainWithOpts mempty $ FBM.testSuite ++ SBM.testSuite
-- main = flip defaultMainWithOpts mempty SBM.testSuite
