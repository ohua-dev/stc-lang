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
simpleComposition v = do
  c <- return v
  r0 <- liftWithIndex 0 foo c
  r1 <- liftWithIndex 1 bar r0
  return r1

simpleSMap v = smap simpleComposition v

returnTest :: Assertion
returnTest = do
  let (result,state) = runOhuaM (return (10::Int)) ([]::[Int])
  assertEqual "result was wrong." (10::Int) result
  assertEqual "state was wrong." ([]::[Int]) state

bindTest :: Assertion
bindTest = do
  let (result,state) = runOhuaM (simpleComposition 10) [0,0]
  assertEqual "result was wrong." 36 result
  assertEqual "state was wrong." [2,3] state

pipeSMapTest :: Assertion
pipeSMapTest = do
  print "Starting test case"
  let (result,state) = runOhuaM (simpleSMap [10,10]) [0,0]
  assertEqual "result was wrong." [36,36] result
  assertEqual "state was wrong." [4,6] state


main :: IO ()
main = defaultMainWithOpts
       [testCase "checking monadic return" returnTest
       ,testCase "checking monadic bind" bindTest
       ,testCase "checking simple pipe smap" pipeSMapTest]
       mempty
