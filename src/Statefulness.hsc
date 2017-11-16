{-# LANGUAGE ForeignFunctionInterface #-}

module Statefulness (c_testfn, Result) where

import Foreign
import Foreign.C.Types

#include "my_state.h"
data Result = Result { s1 :: CInt, s2 :: CInt} deriving (Show)

instance Storable Result where
    sizeOf    _ = (#size my_state)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        s1' <- (#peek my_state, s1) ptr
        s2' <- (#peek my_state, s2) ptr
        return  Result { s1 = s1', s2 = s2' }
    poke ptr (Result s1' s2') = do
        (#poke my_state, s1) ptr s1'
        (#poke my_state, s2) ptr s2'

foreign import ccall "my_state.h testfn" c_testfn :: Int -> Int -> Ptr Result -> IO Int
