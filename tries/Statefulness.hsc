{-# LANGUAGE ForeignFunctionInterface #-}

module Statefulness (Result(..),
                     c_testfn,
                     c_f1,
                     c_f2,
                     c_f3) where

import Foreign

#include "my_state.h"
data Result = Result { val :: Int32, state :: Int32} deriving (Show)

instance Storable Result where
    sizeOf    _ = (#size my_state)
    alignment _ = alignment (undefined :: Int32)
    peek ptr = do
        val' <- (#peek my_state, val) ptr
        state' <- (#peek my_state, state) ptr
        return  Result { val = val', state = state' }
    poke ptr (Result val' state') = do
        (#poke my_state, val) ptr val'
        (#poke my_state, state) ptr state'

foreign import ccall "my_state.h testfn" c_testfn :: Int -> Int -> Ptr Result -> IO Int

foreign import ccall "my_state.h f1" c_f1 :: Int -> Int -> Ptr Result -> IO Int
foreign import ccall "my_state.h f2" c_f2 :: Int -> Int -> Ptr Result -> IO Int
foreign import ccall "my_state.h f3" c_f3 :: Int -> Int -> Ptr Result -> IO Int
