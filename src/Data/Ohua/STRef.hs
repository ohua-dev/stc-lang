{-# LANGUAGE TupleSections #-}

module Data.Ohua.STRef where

import GHC.IORef
import Control.Monad.Ohua.ST

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

-- | This implementation actually used untied state underneath and ties it again differently.
data STRef s a = STRef (IORef a)
-- ^ a value of type @STRef s a@ is a mutable variable in state thread @s@,
-- containing a value of type @a@
--
-- >>> :{
-- runST (do
--     ref <- newSTRef "hello"
--     x <- readSTRef ref
--     writeSTRef ref (x ++ "world")
--     readSTRef ref )
-- :}
-- "helloworld"

-- |Build a new 'STRef' in the current state thread
newSTRef :: a -> ST1 s (STRef s a)
newSTRef init' = ST1 $ STRef <$> newIORef init'

-- |Read the value of an 'STRef'
readSTRef :: STRef s a -> ST1 s a
readSTRef (STRef var) = ST1 $ readIORef var

-- |Write a new value into an 'STRef'
writeSTRef :: STRef s a -> a -> ST1 s ()
writeSTRef (STRef var) val = ST1 $ writeIORef var val

-- | Pointer equality.
--
-- @since 2.01
-- instance Eq (STRef s a) where
--     STRef v1# == STRef v2# = isTrue# (sameMutVar# v1# v2#)

-- setState :: STRef s state -> ST s ()
-- setState state = ST $ \_ -> return (SafetyToken state, ())
--
-- ask' :: ST s state
-- ask' = ST $ \(SafetyToken state) -> return (SafetyToken state, state)

-- ask :: a -> ST s (STRef s a)
-- ask input = ST $ \state -> case (traceShowId state) of
--                           (SafetyToken undefined) -> (state,) . STRef <$> newIORef input
--                           (SafetyToken s) -> return (state, unsafeCoerce s :: STRef s a)
