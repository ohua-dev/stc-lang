import Control.Monad.State

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Data.Monoid
import Control.Monad
-- import Utils

import StreamsBasedMonad

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

simpleComposition v = do
  c <- return v
  r0 <- liftWithIndex "foo" 0 foo 0 c
  r1 <- liftWithIndex "bar" 1 bar 0 r0
  return r1

-- simpleSMap v = smap simpleComposition v
--
-- smapWithContext v = do
--   c <- return v
--   r0 <- liftWithIndex 2 foo c
--   r1 <- liftWithIndex 3 bar r0
--   r2 <- smap simpleComposition [r0,r1]
--   return r2
--
-- smapResultUsed v = do
--   c <- return v
--   r0 <- liftWithIndex 2 foo c
--   r1 <- liftWithIndex 3 bar r0
--   r2 <- smap simpleComposition [r0,r1]
--   r3 <- liftWithIndex 4 foo $ r2 !! 0
--   r4 <- liftWithIndex 5 bar $ r2 !! 1
--   return (r3,r4)


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

-- pipeSMapTest :: Assertion
-- pipeSMapTest = do
--   let (result,state) = runOhuaM (simpleSMap [10,10]) [0,0]
--   assertEqual "result was wrong." [36,36] result
--   assertEqual "state was wrong." [4,6] state
--
-- smapContextTest :: Assertion
-- smapContextTest = do
--   let (result,state) = runOhuaM (smapWithContext 10) [0,0,0,0]
--   assertEqual "result was wrong." [42,114] result
--   assertEqual "state was wrong." [4,6,2,3] state
--
-- smapResultUsedTest :: Assertion
-- smapResultUsedTest = do
--   let (result,state) = runOhuaM (smapResultUsed 10) [0,0,0,0,0,0]
--   assertEqual "result was wrong." (44,342) result
--   assertEqual "state was wrong." [4,6,2,3,2,3] state


main :: IO ()
main = defaultMainWithOpts
       [testCase "checking monadic return" returnTest
       ,testCase "checking monadic bind" bindTest
       -- ,testCase "checking simple pipe smap" pipeSMapTest
       -- ,testCase "checking smap with context" smapContextTest
       -- ,testCase "checking smap result used" smapResultUsedTest
       ]
       mempty
