{-# LANGUAGE BangPatterns #-}

module FakeComputation where

-- | Original source https://github.com/iu-parfunc/lvars/tree/master/archived_old/fhpc13-lvars/benchmarks

import           Control.Exception (evaluate)

import Control.DeepSeq
import Control.Monad.State

-- Iterates the sin function n times on its input and returns the sum
-- of all iterations.
sin_iter :: Int -> Float -> Float
sin_iter 0  x = x
sin_iter n !x = sin_iter (n - 1) (x + sin x)


wrk_sins :: Int -> Float -> Float
wrk_sins num sin_wrk =
  let res = sin_iter num (2.222 + sin_wrk)
  in force res

type ID = Int

work :: Float -> StateT (ID,Int) IO (Float, Float)
work wrk = do
  (identifier,numIter) <- get
  -- liftIO $ putStrLn $ "start: " ++ (show identifier)
  let r = wrk_sins numIter wrk
  -- liftIO $ putStrLn $ "stop: " ++ (show identifier)
  return (wrk,r)
