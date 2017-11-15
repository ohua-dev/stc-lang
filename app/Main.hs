module Main where

import Foreign
import Foreign.Ptr
import Statefulness
import Debug.Trace

main :: IO ()
main = alloca $ \resultPtr -> do
  err <- c_testfn 5 10 resultPtr
  r <- peek resultPtr
  return $ traceShow r ()
