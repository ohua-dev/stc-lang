
module STMonadStateThreads where

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit

import Control.Monad.Ohua.ST
import Data.Ohua.STRef
-- import Control.Monad.ST
-- import Control.Monad.Reader
-- import Data.STRef


-- state preparation
-- prepare :: ST s ()
-- prepare = do
--   ref <- newSTRef 0
--   setState ref
--   return ()

prepare1 :: ST1 s (STRef s Integer)
prepare1 = newSTRef 0

-- mySimpleStateThread :: StateThread Integer s Integer Integer
-- mySimpleStateThread :: Integer -> ST1 s Integer
-- mySimpleStateThread i = do
--   state <- ask 0
--   current <- readSTRef state
--   let current' = current + 10 + i
--   writeSTRef state current'
--   return current'

mySimpleStateThread1 :: STRef s Integer -> Integer -> ST1 s Integer
mySimpleStateThread1 state i = do
  current <- readSTRef state
  let current' = current + 10 + i
  writeSTRef state current'
  return current'

simpleTest :: Assertion
simpleTest =
  let stateThread = makeST1 (InitSF1 prepare1 mySimpleStateThread1)
      -- stateThread = makeST0 (prepare1, mySimpleStateThread1)
      result1     = stateThread 5
      result2    = stateThread 15
      -- _ = makeST1 (InitSF (pure ()) (\_ _ -> newSTRef ()))
  in do
    assertEqual "first" 15 result1
    assertEqual "second" 40 result2

testSuite :: Test.Framework.Test
testSuite =
    testGroup
        "ST"
        [ testCase "checking ST state threads" simpleTest ]

-- main :: IO ()
-- main = defaultMainWithOpts
--        [ testCase "checking ST state threads" simpleTest ]
--        mempty
