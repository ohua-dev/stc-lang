{-# LANGUAGE BangPatterns #-}

module FakeComputation where

-- | Original source https://github.com/iu-parfunc/lvars/tree/master/archived_old/fhpc13-lvars/benchmarks

import           Control.Exception (evaluate)

import Control.DeepSeq
import Control.Monad.State

import Monad.StreamsBasedFreeMonad

import Control.Concurrent (myThreadId)


-- Iterates the sin function n times on its input and returns the sum
-- of all iterations.
sin_iter :: Int -> Float -> Float
sin_iter 0  x = x
sin_iter n !x = sin_iter (n - 1) (x + sin x)

cos_iter :: Int -> Float -> Float
cos_iter 0  x = x
cos_iter n !x = cos_iter (n - 1) (x + cos x)

tan_iter :: Int -> Float -> Float
tan_iter 0  x = x
tan_iter n !x = tan_iter (n - 1) (x + tan x)

wrk_sins :: Int -> Float -> Float
wrk_sins num sin_wrk =
  let res = sin_iter num (2.222 + sin_wrk)
  in force res

gwrk :: Int -> Float -> (Int -> Float -> Float) -> Float
gwrk num wrk f =
  let res = f num (2.222 + wrk)
  in force res

type ID = Int

work :: Float -> StateT (ID,Int) IO (Float, Float)
work wrk = do
  -- liftIO $ putStrLn $ "work: " ++ (show wrk)
  (identifier,numIter) <- get
  tId <- liftIO myThreadId
  -- liftIO $ putStrLn $ "start: " ++ (show identifier) ++ " on thread: " ++ (show tId)
  -- let r = wrk_sins numIter wrk
  r <- liftIO $ evaluate $ force $ sin_iter numIter (2.222 + wrk) -- this is the solution!
  -- liftIO $ putStrLn $ "stop: " ++ (show identifier)
  -- liftIO $ putStrLn $ "result: " ++ (show r)
  return (wrk,r)

gwork :: (Int -> Float -> Float) -> Float -> StateT (ID,Int) IO (Float, Float)
gwork f wrk = do
  -- liftIO $ putStrLn $ "work: " ++ (show wrk)
  (identifier,numIter) <- get
  tId <- liftIO myThreadId
  -- liftIO $ putStrLn $ "start: " ++ (show identifier) ++ " on thread: " ++ (show tId)
  -- let r = gwrk numIter wrk f
  r <- liftIO $ evaluate $ force $ f numIter (2.222 + wrk) -- this is the solution!
  -- liftIO $ putStrLn $ "stop: " ++ (show identifier)
  -- liftIO $ putStrLn $ "result: " ++ (show r)
  return (wrk,r)
