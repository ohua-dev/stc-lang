module Main where

import Foreign
import Foreign.Ptr
-- import Statefulness
import Sfc
import Control.Monad.State

-- main :: IO ()
-- main = alloca $ \resultPtr -> do
--   err <- c_testfn 3 10 resultPtr
--   r <- peek resultPtr
--   print $ "result:" ++ show r

-- main :: IO ()
-- main = let xs = [0..9]
--            zero = 0 :: Int
--            s = S zero zero zero
--        in do
--         print "With ffi:"
--         print $ runState (smap (f3'.@.f2'.@.f1') xs) s
--         print "Without ffi:"
--         print $ runState (smap (f3.@.f2.@.f1) xs) s

main :: IO ()
main = let xs = [0..9]
           zero = 0 :: Int
           s = S zero zero zero
       in do
        print "With ffi:"
        print $ runState (smap (f2'.@.f1') xs) s
