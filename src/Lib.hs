module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ c_testfn 5 10
