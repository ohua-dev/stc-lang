import Control.Monad.State

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Data.Monoid
import Control.Monad
-- import Utils

import FuturesBasedMonad

foo :: Int -> State Int Int
foo x = do
  s <- get
  put $ s+2
  return $ x+2

bar :: Int -> State Int Int
bar x = do
  s <- get
  put $ s+3
  return $ x*3

-- simpleAlgo :: Int -> OhuaM ([LocalStateBox Int], [LocalStateBox Int]) Int
simpleAlgo v = do
  c <- return v
  r0 <- liftWithIndex 0 foo c
  r1 <- liftWithIndex 1 bar r0
  return r1

simpleTest :: Assertion
simpleTest = do
  print "Starting test case"
  print $ runOhuaM (simpleAlgo 10) [0,0]

main :: IO ()
main = defaultMainWithOpts
       [testCase "simple" simpleTest]
       mempty
