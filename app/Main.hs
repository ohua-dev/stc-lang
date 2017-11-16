module Main where

import Foreign
import Foreign.Ptr
import Statefulness

main :: IO ()
main = alloca $ \resultPtr -> do
  err <- c_testfn 3 10 resultPtr
  r <- peek resultPtr
  print $ "result:" ++ show r
