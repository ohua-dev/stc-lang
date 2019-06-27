
module SD.Correctness where

import Control.Monad.State
import Control.Exception

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Data.Monoid
-- import Utils

import Control.Monad.SD
import Data.StateElement

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

barFloat :: Int -> StateT Float IO Int
barFloat x = do
  s <- get
  put $ s+3.34
  return $ x*3

-- simpleAlgo :: Int -> OhuaM ([LocalStateBox Int], [LocalStateBox Int]) Int
simpleComposition v = do
  c <- return v
  r0 <- liftWithIndex 0 foo c
  r1 <- liftWithIndex 1 bar r0
  return r1

packagedSimpleComposition = do
  f0 <- liftWithState (return 0) foo
  f1 <- liftWithState (return 0) bar
  return $ f1 <=< f0

simpleCompositionHetState v = do
  c <- return v
  r0 <- liftWithIndex 0 foo c
  r1 <- liftWithIndex 1 barFloat r0
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


packagedSmapResultUsed = do
  f0 <- liftWithState (return 0) foo
  f1 <- liftWithState (return 0) bar
  f2 <- smapSTC packagedSimpleComposition
  f3 <- liftWithState (return 0) foo
  f4 <- liftWithState (return 0) bar
  return $ \v -> do
      r0 <- f0 v
      r1 <- f1 r0
      r2 <- f2 [r0,r1]
      r3 <- f3 $ r2 !! 0
      r4 <- f4 $ r2 !! 1
      return (r3,r4)

smapOverEmptyList = do
  r1 <- smap simpleComposition []
  smap someComp [length r1]
  where
    someComp i = do
        r0 <- liftWithIndex 2 foo i
        liftWithIndex 3 bar r0

smapOverEmptyList2 =
    smap (smap simpleComposition) [[], [1..3]]

simpleCompositionPackaged v = do
  c <- return v
  r0 <- liftWithIndex 0 foo c
  r1 <- liftWithIndex 1 bar r0
  return r1

caseComp idxFoo idxBranch1 idxBranch2 v = do
  c <- liftWithIndex idxFoo foo v
  o <- case_ c [
                 (4, branch1 c)
               , (8, branch2 c)
               ]
  return o
  where
    branch1 = liftWithIndex idxBranch1 bar
    branch2 = liftWithIndex idxBranch2 bar

caseComposition = caseComp 0 1 2

smapWithCase = smap caseComposition

nestedCase v = do
  o <- case_ v [
                 (2, caseComp 0 1 2 v)
               , (6, caseComp 3 4 5 v)
               ]
  return o

ret = return (10::Int)

returnTest :: Assertion
returnTest = do
  (result,s) <- runOhuaM ret []
  assertEqual "result was wrong." (10::Int) result
  assertEqual "state was wrong." [] (map fromS s :: [Int])

bindTest :: Assertion
bindTest = do
  (result,s) <- runOhuaM (simpleComposition 10) $ map toS [0::Int,0]
  assertEqual "result was wrong." 36 result
  assertEqual "state was wrong." [2,3] (map fromS s :: [Int])

hetStateTest :: Assertion
hetStateTest = do
  (result,s1:(s2:_)) <- runOhuaM (simpleCompositionHetState 10) [ toS (0::Int)
                                                            , toS (2.5::Float)
                                                            ]
  assertEqual "result was wrong." 36 result
  assertEqual "state was wrong." 2 (fromS s1 :: Int)
  assertEqual "state was wrong." 5.84 (fromS s2 :: Float)
  -- assertEqual "state was wrong." 2 (toConcrete s1 :: Int)
  -- assertEqual "state was wrong." 5.84 (toConcrete s2 :: Float)

pipeSMapTest :: Assertion
pipeSMapTest = do
  (result,s) <- runOhuaM (simpleSMap [10,10]) $ map toS [0::Int,0]
  assertEqual "result was wrong." [36,36] result
  assertEqual "state was wrong." [4,6] (map fromS s :: [Int])

smapContextTest :: Assertion
smapContextTest = do
  (result,s) <- runOhuaM (smapWithContext 10) $ map toS [0::Int,0,0,0]
  assertEqual "result was wrong." [42,114] result
  assertEqual "state was wrong." [4,6,2,3] (map fromS s :: [Int])

smapResultUsedTest :: Assertion
smapResultUsedTest = do
  (result,s) <- runOhuaM (smapResultUsed 10) $ map toS [0::Int,0,0,0,0,0]
  assertEqual "result was wrong." (44,342) result
  assertEqual "state was wrong." [4,6,2,3,2,3] (map fromS s :: [Int])


-- | Basically the same as 'smapResultUsed' but order of elements in the state
-- is different, because of the order in which the state monad collects the
-- indices.
packagedSmapResultUsedTest :: Assertion
packagedSmapResultUsedTest = do
  (result,s) <- runSTCLang packagedSmapResultUsed 10
  assertEqual "result was wrong." (44,342) result
  assertEqual "state was wrong." [2,3, 4, 6,2,3] (map fromS s :: [Int])

packagedBindTest :: Assertion
packagedBindTest = do
  (result,s) <- runOhuaM (simpleCompositionPackaged 10) $ map toS [0::Int,0]
  assertEqual "result was wrong." 36 result
  assertEqual "state was wrong." [2,3] (map fromS s :: [Int])

caseTest :: Assertion
caseTest = do
  -- "true" branch
  (result,s) <- runOhuaM (caseComposition 2) $ map toS [0::Int,0,0]
  assertEqual "result was wrong." 12 result
  assertEqual "state was wrong." [2,3,0] (map fromS s :: [Int])

  -- "false" branch
  (result',s') <- runOhuaM (caseComposition 6) $ map toS [0::Int,0,0]
  assertEqual "result was wrong." 24 result'
  assertEqual "state was wrong." [2,0,3] (map fromS s' :: [Int])

caseSmapTest :: Assertion
caseSmapTest = do
  -- "true" branch
  (result,s) <- runOhuaM (smapWithCase [2,6]) $ map toS [0::Int,0,0]
  assertEqual "result was wrong." [12,24] result
  assertEqual "state was wrong." [4,3,3] (map fromS s :: [Int])
  -- execute only once
  (result,s) <- runOhuaM (smapWithCase [2]) $ map toS [0::Int,0,0]
  assertEqual "result was wrong." [12] result
  --assertEqual "state was wrong." [4,3,3] (map fromS s :: [Int])

nestedCaseTest :: Assertion
nestedCaseTest = do
  -- "true" branch
  (result,s) <- runOhuaM (nestedCase 2) $ map toS [0::Int,0,0,0,0,0]
  assertEqual "result was wrong." 12 result
  assertEqual "state was wrong." [2,3,0,0,0,0] (map fromS s :: [Int])
  -- "false" branch
  (result',s') <- runOhuaM (nestedCase 6) $ map toS [0::Int,0,0,0,0,0]
  assertEqual "result was wrong." 24 result'
  assertEqual "state was wrong." [0,0,0,2,0,3] (map fromS s' :: [Int])

tooMuchStateTest :: Assertion
tooMuchStateTest = do
  (result,s) <- runOhuaM ret $ map toS [0::Int]
  assertEqual "result was wrong." (10::Int) result
  assertEqual "state was wrong." [0] (map fromS s :: [Int])

notEnoughStateTest :: Assertion
notEnoughStateTest = do
  (result,s) <- runOhuaM (simpleComposition 10) $ map toS [0::Int]
  assertEqual "result was wrong." 36 result
  assertEqual "state was wrong." [2,3] (map fromS s :: [Int])

smapHandlesEmptyList :: Assertion
smapHandlesEmptyList =
    void (runOhuaM smapOverEmptyList (map toS [0 .. 3 :: Int]))

smapHandlesEmptyList2 :: Assertion
smapHandlesEmptyList2 = do
    (res, _) <-
        runOhuaM smapOverEmptyList2 (map toS [0 .. 1 :: Int])
    assertEqual "lengths differ" [0, 3] (map length res)


testSuite :: Test.Framework.Test
testSuite =
    testGroup
        "Futures"
        [ testCase "checking monadic return" returnTest
        , testCase "checking monadic bind" bindTest
        , testCase "checking simple pipe smap" pipeSMapTest
        , testCase "checking smap with context" smapContextTest
        , testCase "checking smap result used" smapResultUsedTest
        , testCase "smap over empty list" smapHandlesEmptyList
        , testCase "nested smap over empty list" smapHandlesEmptyList2
        , testCase "checking packaged version" packagedBindTest
        , testCase "checking case statement" caseTest
        , testCase "checking smap-case composition" caseSmapTest
        , testCase "simple nested case composition" nestedCaseTest
        , testCase "heterogeneous state" hetStateTest
        , testCase "test packaged state" packagedSmapResultUsedTest
            -- , testCase "Futures: too much state" tooMuchStateTest --> this turns into an Error in monad-par that says: "no result"
            -- , testCase "Futures: not enough state" notEnoughStateTest --> turns into the error: Prelude.!!: index too large
        ]
