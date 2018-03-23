{-# LANGUAGE FlexibleContexts #-}

module CorrectnessStreamsBasedMonad where

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit

import Monad.StreamsBasedFreeMonad
import Monad.StreamsBasedExplicitAPI
import Data.Dynamic2

import Control.Monad.State

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

cons_ :: Int -> Int -> StateT Int IO [Int]
cons_ x y = return [x,y]

at_ :: [Int] -> Int -> StateT Int IO Int
at_ xs ith = return $ xs !! ith

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
  r01 <- lift2WithIndex 4 cons_ r0 r1
  r2 <- smap simpleComposition r01
  return r2

smapResultUsed v = do
  c <- return v
  r0 <- liftWithIndex 2 foo c
  r1 <- liftWithIndex 3 bar r0
  r01 <- lift2WithIndex 4 cons_ r0 r1
  r2 <- smap simpleComposition r01
  r20 <- lift2WithIndex 5 at_ r2 =<< sfConst' 0
  r21 <- lift2WithIndex 6 at_ r2 =<< sfConst' 1
  r3 <- liftWithIndex 7 foo r20
  r4 <- liftWithIndex 8 bar r21
  lift2WithIndex 9 cons_ r3 r4


-- returnTest :: Assertion
-- returnTest = do
--   -- FIXME API not correct!
--   -- (result,s) <- runAlgo (return (10::Int)) ([]::[Int])
--   result <- runOhuaM (return (10::Int)) ([]::[Int])
--   assertEqual "result was wrong." (10::Int) result
--   -- assertEqual "state was wrong." ([]::[Int]) s

bindTest :: Assertion
bindTest = do
  -- FIXME API not correct!
  result <- runOhuaM (simpleComposition =<< sfConst' 10) $ map toDyn [0::Int,0]
  assertEqual "result was wrong." 36 result
  -- assertEqual "state was wrong." [2,3] s

pipeSMapTest :: Assertion
pipeSMapTest = do
  -- FIXME API not correct!
  result <- runOhuaM (simpleSMap =<< sfConst' [10,10]) $ map toDyn [0::Int,0,0]
  assertEqual "result was wrong." [36,36] result
  -- assertEqual "state was wrong." [4,6] s

smapContextTest :: Assertion
smapContextTest = do
  -- FIXME API not correct!
  result <- runOhuaM (smapWithContext =<< sfConst' 10) $ map toDyn [0::Int,0,0,0,0]
  assertEqual "result was wrong." [42,114] result
  -- assertEqual "state was wrong." [4,6,2,3] s

smapResultUsedTest :: Assertion
smapResultUsedTest = do
  -- FIXME API not correct!
  result <- runOhuaM (smapResultUsed =<< sfConst' 10) $ map toDyn [0::Int,0,0,0,0,0,0,0,0,0]
  assertEqual "result was wrong." [44,342] result
  -- assertEqual "state was wrong." [4,6,2,3,2,3] s

testSuite :: Test.Framework.Test
testSuite =
    testGroup
        "Streams"
              -- testCase "Streams: checking monadic return" returnTest
        [ testCase "checking monadic bind" bindTest
        , testCase "checking simple pipe smap" pipeSMapTest
        , testCase "checking smap with context" smapContextTest
        , testCase "checking smap result used" smapResultUsedTest
        ]
