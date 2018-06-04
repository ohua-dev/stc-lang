{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Control.Monad.Ohua.ST where

import System.IO.Unsafe (unsafePerformIO)
-- import Control.Monad.ST.Unsafe (unsafeSTToIO)

-- import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad
import Control.Applicative

import Control.Monad.ST

-- import Data.Ohua.STRef (STRef)
-- import Data.IORef

-- data SafetyToken state s = SafetyToken state

-- data SafetyToken state s a b = SafetyToken {
--     cont :: a -> (b, SafetyToken state s a b)
--   }

-- newtype ST s a = ST (STRep s a)
-- type STRep s a = forall state.SafetyToken state s -> IO (SafetyToken state s, a)
--
newtype ST1 s a = ST1 {
  runST1 :: IO a
} deriving (Functor, Applicative, Monad)

data InitSF1 s a b = forall state. InitSF1 (ST1 s state) (state -> a -> ST1 s b)

data InitSF s a b = forall state. InitSF (ST s state) (state -> a -> ST s b)

-- instance Functor (ST s) where
--     fmap f (ST m) = ST $ \ s -> do
--       (new_s, r ) <- m s
--       return ( new_s, f r)
--
-- -- | @since 4.4.0.0
-- instance Applicative (ST s) where
--     {-# INLINE pure #-}
--     {-# INLINE (*>)   #-}
--     pure x = ST (\ s -> return ( s, x ))
--     m *> k = m >>= \ _ -> k
--     (<*>) = ap
--     liftA2 = liftM2
--
-- -- | @since 2.01
-- instance Monad (ST s) where
--     {-# INLINE (>>=)  #-}
--     (>>) = (*>)
--     (ST m) >>= k = ST $ \ s -> do
--                             ( new_s, r ) <- m s
--                             let (ST k2) = k r
--                             k2 new_s

-- instance Functor (ST s) where
--     fmap f (ST m) = ST $ \ s ->
--       case (m s) of { (new_s, r ) ->
--         ( new_s, f r) }
--
-- -- | @since 4.4.0.0
-- instance Applicative (ST s) where
--     {-# INLINE pure #-}
--     {-# INLINE (*>)   #-}
--     pure x = ST (\ s -> ( s, x ))
--     m *> k = m >>= \ _ -> k
--     (<*>) = ap
--     liftA2 = liftM2
--
-- -- | @since 2.01
-- instance Monad (ST s) where
--     {-# INLINE (>>=)  #-}
--     (>>) = (*>)
--     (ST m) >>= k
--       = ST (\ s ->
--         case (m s) of { ( new_s, r ) ->
--         case (k r) of { ST k2 ->
--         (k2 new_s) }})

-- unsafeRunST :: ST s a -> state -> a
-- unsafeRunST (ST comp) = snd . unsafePerformIO . comp . SafetyToken
--
-- unsafeRunST' :: ST s a -> state -> (state, a)
-- unsafeRunST' (ST comp) state =
--   let (SafetyToken state', result) = unsafePerformIO . comp $ SafetyToken state
--   in (state', result)
--
-- unsafeRunST'' :: ST s () -> state
-- unsafeRunST'' (ST comp) =
--   let (SafetyToken state, _) = unsafePerformIO . comp $ SafetyToken undefined
--   in state
--
-- newtype Cont a b = Cont {
--   run :: a -> (b, Cont a b)
-- }

-- FIXME it is not possible to keep something safe via the type-level and still have the caller specify its type.
--       note carefully how the ST monad does it: the type of the state in an ST monad is never really stated anywhere.
--       it is created implicitly via the calls to newSTRef etc.
--       this is exactly the way that we have to do the state stuff! it needs to be part of the API for our StateThread
--       such that the caller does not have to state the type of that state at all!
--       in addition to that calls of the ST monad, we have to provide a call such as 'state 5 :: ST s STRef Integer'
-- makeST :: forall a b.(forall s state.(ST s state, state -> a -> ST s b)) -> Cont a b
-- makeST (initialState, stateThread) = Cont $ f $ unsafeRunST'' initialState undefined
--   where
--     -- f :: state -> a -> (b, Cont a b)
--     f state input =
--       let comp   = stateThread input
--           (state', result) = unsafeRunST' comp state
--           -- comp'  = comp state
--           -- -- result = runST comp' -- does not work due to escaping state
--           -- result = unsafeRunST comp' -- need to enforce encapsulation on my own
--       in
--         (result, Cont $ f state')


-- • Couldn't match type ‘state’ with ‘STRef s Integer’
--       ‘state’ is a rigid type variable bound by
--         a type expected by the context:
--           forall s state. (ST s state, StateThread state s Integer Integer)
--         at test/STMonadStateThreads.hs:30:21-57
--       Expected type: ST s state
--         Actual type: ST s (STRef s Integer)
--     • In the expression: prepare
--       In the first argument of ‘makeST’, namely
--         ‘(prepare, mySimpleStateThread)’
--       In the expression: makeST (prepare, mySimpleStateThread)
--    |
-- 30 |   let stateThread = makeST (prepare, mySimpleStateThread)
--    |                             ^^^^^^^
makeST0 :: forall a b.(forall s state.(ST1 s state, state -> a -> ST1 s b)) -> a -> b
makeST0 (initialState, stateThread) = unsafePerformIO $ do
  state <- runST1 initialState
  return $ \ a -> unsafePerformIO $ runST1 $ stateThread state a
-- does not seem to work because we want to say that the type of 'state' depends
-- on the type subsituted in the 'state'  type variable in 'ST1 s state'.

-- Prelude> newtype ST1 s a = ST1 (IO a)
-- Prelude> data InitSF1 s a b = forall state. InitSF1 (ST1 s state) (state -> a -> ST1 s b)
-- Prelude> :t InitSF1
-- InitSF1 :: ST1 s state -> (state -> a -> ST1 s b) -> InitSF1 s a b

-- the point is that (,) is no capable of stating that the two 'state' type variables
-- actually refer to the same type. that is because the type is:
-- (,) :: a -> b -> (a,b)
-- it does not handle these type parameters explicitly.
-- so I guess the following will happen:
-- (prepare, mySimpleStateThread) :: ST1 s (STRef s Integer) -> ((STRef s Integer) -> a -> ST1 s b) -> (ST1 s (STRef s Integer), (STRef s Integer)-> a -> ST1 s b)
-- but the passed in functon and type are supposed to be generic in 's' and 'state'!
-- this is true for 's' but not for 'state'!
-- THIS is the point why the InitSF1-approach works because it 'existentials-away' the
-- 'state' type variable because the InitSF1 type does not have it.
-- However, the existential still allows to express that the 'state' is the same for
-- the data type and the function. It hides this state aspect on the type system level wherever InitSF is being used.
-- Really cool!

-- works
makeST1 :: forall a b.(forall s.InitSF1 s a b) -> a -> b
makeST1 (InitSF1 initialState stateThread) = unsafePerformIO $ do
  state <- runST1 initialState
  return $ \ a -> unsafePerformIO $ runST1 $ stateThread state a

-- TODO
-- runStateThread :: (forall t. (InitSF1 t a b, Tagged t [a])) -> [b]
-- runStateThread (InitSF1 initialState stateThread) =
--
-- makeST12 :: forall a b.(forall s.InitSF1 s a b) -> a -> b
-- makeST12 (InitSF1 initialState stateThread) = unsafePerformIO $ do
--   state <- runST1 initialState
--   return $ \ a -> unsafePerformIO $ runST1 $ stateThread state a
--
-- data Cont a b = forall s. Cont (a -> ST s b)
--
-- makeST :: forall a b.(forall s.InitSF s a b) -> Cont a b
-- makeST (InitSF initialState stateThread) = runST $ do
--   ist <- initialState
--   return $ Cont $ stateThread ist


  -- Cont $ f $ unsafeRunST'' initialState undefined
  -- where
  --   -- f :: state -> a -> (b, Cont a b)
  --   f state input =
  --     let comp   = stateThread input
  --         (state', result) = unsafeRunST' comp state
  --         -- comp'  = comp state
  --         -- -- result = runST comp' -- does not work due to escaping state
  --         -- result = unsafeRunST comp' -- need to enforce encapsulation on my own
  --     in
  --       (result, Cont $ f state')

-- unsafeRunST :: ST s a -> a
-- unsafeRunST = unsafePerformIO . unsafeSTToIO

-- early notes:

-- approach: make the call to create the state unsafe and then keep the
-- state reference to perform only safe calls.

-- note that the "s" in ST s a is just type-level trickery:
-- data STRef s a = STRef (MutVar# s a)
-- newtype ST s a = ST (STRep s a)
-- type STRep s a = State# s -> (# State# s, a #)
-- both MutVar# and State# are builtin types/functions which means that there is
-- no constructor that requires an argument of type s.
-- this is all done internally by the GHC, i.e., the GHC will provide this argument.
-- one can see it as a tag of the current state.
--
-- check:
--
-- GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
-- Prelude> import Control.Monad.ST
-- Prelude Control.Monad.ST> import Data.STRef
-- Prelude Control.Monad.ST Data.STRef> newSTRef "hello"
-- <<ST action>>
-- Prelude Control.Monad.ST Data.STRef> :t newSTRef "hello"
-- newSTRef "hello" :: ST s (STRef s [Char])
-- Prelude Control.Monad.ST Data.STRef> test = newSTRef "hello" >>= (\r -> readSTRef r >>= (\s -> return $ s ++ " test"))
-- Prelude Control.Monad.ST Data.STRef> runST test
-- "hello test"
-- Prelude Control.Monad.ST Data.STRef> :t test
-- test :: ST s [Char]
-- Prelude Control.Monad.ST Data.STRef> run = runST test
-- Prelude Control.Monad.ST Data.STRef> :t run
-- run :: [Char]
-- Prelude Control.Monad.ST Data.STRef>
--
-- the "s" really never gets a concrete type.
