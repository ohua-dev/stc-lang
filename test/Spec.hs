
import           Test.Framework

import           CorrectnessFuturesBasedMonad   as FBM
import           CorrectnessStreamsBasedMonad   as SBM
import           Monad.StreamsBasedFreeMonad
import           Test.Framework.Providers.HUnit
import           Test.HUnit

main :: IO ()
main =
  flip defaultMainWithOpts mempty $
  FBM.testSuite
  ++ SBM.testSuite
  ++ [ testCase "using if" $ do
         let a = do
               four <- sfConst (4 :: Int)
               five <- sfConst 5
               isGreater <- (four `gt` five)
               if_ isGreater
                 (sfConst True)
                 (sfConst False)
         result <- flip runAlgo () =<< createAlgo a
         assertEqual "If selected incorrect branch" result False
     ]
-- main = flip defaultMainWithOpts mempty FBM.testSuite
