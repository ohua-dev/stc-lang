
module CorrectnessFuturesBasedMonad where

import Control.Monad.State

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Data.Monoid
-- import Utils

import FuturesBasedMonad

foo :: Int -> StateT Int IO Int
foo x = do
  s <- get
  put $ s+2
  return $ x+2

bar :: Int -> StateT Int IO Int
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

simpleSMap = smap simpleComposition

smapWithContext v = do
  c <- return v
  r0 <- liftWithIndex 2 foo c
  r1 <- liftWithIndex 3 bar r0
  r2 <- smap simpleComposition [r0,r1]
  return r2

smapResultUsed v = do
  c <- return v
  r0 <- liftWithIndex 2 foo c
  r1 <- liftWithIndex 3 bar r0
  r2 <- smap simpleComposition [r0,r1]
  r3 <- liftWithIndex 4 foo $ r2 !! 0
  r4 <- liftWithIndex 5 bar $ r2 !! 1
  return (r3,r4)

simpleCompositionPackaged v = do
  c <- return v
  r0 <- liftWithIndex' 0 $ foo c
  r1 <- liftWithIndex' 1 $ bar r0
  return r1

caseComposition v = do
  c <- liftWithIndex 0 foo v
  o <- case_ c [
                 (4, branch1 c)
               , (8, branch2 c)
               ]
  return o
  where
    branch1 = liftWithIndex 1 bar
    branch2 = liftWithIndex 2 bar

smapWithCase = smap caseComposition

returnTest :: Assertion
returnTest = do
  (result,s) <- runOhuaM (return (10::Int)) ([]::[Int])
  assertEqual "result was wrong." (10::Int) result
  assertEqual "state was wrong." ([]::[Int]) s

bindTest :: Assertion
bindTest = do
  (result,s) <- runOhuaM (simpleComposition 10) [0,0]
  assertEqual "result was wrong." 36 result
  assertEqual "state was wrong." [2,3] (s::[Int])

pipeSMapTest :: Assertion
pipeSMapTest = do
  (result,s) <- runOhuaM (simpleSMap [10,10]) [0,0]
  assertEqual "result was wrong." [36,36] result
  assertEqual "state was wrong." [4,6] (s::[Int])

smapContextTest :: Assertion
smapContextTest = do
  (result,s) <- runOhuaM (smapWithContext 10) [0,0,0,0]
  assertEqual "result was wrong." [42,114] result
  assertEqual "state was wrong." [4,6,2,3] (s::[Int])

smapResultUsedTest :: Assertion
smapResultUsedTest = do
  (result,s) <- runOhuaM (smapResultUsed 10) [0,0,0,0,0,0]
  assertEqual "result was wrong." (44,342) result
  assertEqual "state was wrong." [4,6,2,3,2,3] (s::[Int])

packagedBindTest :: Assertion
packagedBindTest = do
  (result,s) <- runOhuaM (simpleCompositionPackaged 10) [0,0]
  assertEqual "result was wrong." 36 result
  assertEqual "state was wrong." [2,3] (s::[Int])

caseTest :: Assertion
caseTest = do
  -- "true" branch
  (result,s) <- runOhuaM (caseComposition 2) [0,0,0]
  assertEqual "result was wrong." 12 result
  assertEqual "state was wrong." [2,3,0] (s::[Int])

  -- "false" branch
  (result',s') <- runOhuaM (caseComposition 6) [0,0,0]
  assertEqual "result was wrong." 24 result'
  assertEqual "state was wrong." [2,0,3] (s'::[Int])

caseSmapTest :: Assertion
caseSmapTest = do
  -- "true" branch
  (result,s) <- runOhuaM (smapWithCase [2,6]) [0,0,0]
  assertEqual "result was wrong." [12,24] result
  assertEqual "state was wrong." [4,3,3] (s::[Int])



testSuite :: [Test.Framework.Test]
testSuite = [
              testCase "Futures: checking monadic return" returnTest
            , testCase "Futures: checking monadic bind" bindTest
            , testCase "Futures: checking simple pipe smap" pipeSMapTest
            , testCase "Futures: checking smap with context" smapContextTest
            , testCase "Futures: checking smap result used" smapResultUsedTest
            , testCase "Futures: checking packegd version" packagedBindTest
            , testCase "Futures: checking case statement" caseTest
            , testCase "Futures: checking smap-case composition" caseSmapTest
            ]
