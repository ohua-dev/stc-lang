{-# LANGUAGE BangPatterns #-}

import Control.Monad.State

import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit
import Control.Monad
import Debug.Trace
import Control.Parallel (pseq)
import GHC.Conc (numCapabilities, getNumCapabilities)
-- import Control.Concurrent.Thread.Delay

import FuturesBasedMonad

compute x msg =
  foldl f 0 $ [x,(x+1)..(x+10000000)] where
    f a b = let c = a + b
                m = b `mod` 1000000 in
                -- c
              case m of
                0 -> (oPrint $ msg ++ ": " ++ show x ++ " : " ++ show b) `pseq` c
                -- 0 -> trace (msg ++ ": " ++ show m ++ " : " ++ show b) c
                otherwise -> c

-- sleep i c = unsafePerformIO $ delay i >> return c

-- p c msg =
--   unsafePerformIO $ (print msg) >> return c

foo :: Int -> State Int Int
foo x = do
  s <- get
  put $ s+2
  return $ compute x "foo"

bar :: Int -> State Int Int
bar x = do
  s <- get
  put $ s+3
  return $ compute x "bar"

foobar :: Int -> State Int Int
foobar x = do
  s <- get
  put $ s+3
  return $ compute x "foobar"

foo' :: Int -> State Int Int
foo' x = do
  s <- get
  let !r = compute x "foo"
  put $ s+2
  return r

bar' :: Int -> State Int Int
bar' x = do
  s <- get
  let !r = compute x "bar"
  put $ s+3
  return r

foobar' :: Int -> State Int Int
foobar' x = do
  s <- get
  let !r = compute x "foobar"
  put $ s+3
  return r

-- simpleComposition v = do
--   c <- return v
--   r0 <- liftWithIndex 0 foo c
--   r1 <- liftWithIndex 1 bar r0
--   r2 <- liftWithIndex 2 foobar r1
--   return r2

simpleComposition' ident v =
  return v >>= (\c->
  (liftWithIndex "foo" 0 foo ident c) >>= (\r0 ->
  (liftWithIndex "bar" 1 bar ident r0) >>= (\r1 ->
  (liftWithIndex "foobar" 2 foobar ident r1))))

simpleComposition'' ident v =
  return v >>= (\c->
  (liftWithIndex "foo" 0 foo' ident c) >>= (\r0 ->
  (liftWithIndex "bar" 1 bar' ident r0) >>= (\r1 ->
  (liftWithIndex "foobar" 2 foobar' ident r1))))

simpleSMap v = smap simpleComposition' v

pipeSMapTest :: Assertion
pipeSMapTest = do
  print $ "numCapabilities: " ++ show numCapabilities
  (\x -> print $ "getNumCapabilities: " ++ show x) =<< getNumCapabilities
  let d = [1, 2, 3, 4]
  -- let (result,state) = runOhuaM (simpleComposition' 0) [0,0,0]
  let (result,state) = runOhuaM (simpleSMap d) [0,0,0]
  -- assertEqual "result was wrong." [36,36] result
  -- assertEqual "state was wrong." [4,6] state
  print $ "result and state:" ++ show (result,state)


main :: IO ()
main = defaultMainWithOpts
       [testCase "3 stage pipeline" pipeSMapTest]
       mempty
