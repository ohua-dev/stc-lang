{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

--- this implementation does not rely on channels. it builds on futures!
module Control.Monad.SD.Ohua
  ( liftWithIndex
  , liftWithIndex'
  , SF
  , SFM
  , runOhuaM
  , OhuaM
  , OhuaM(..)
  , GlobalState
  , GlobalState(..)
  ) where

import Control.Monad

-- import           Control.Monad.Par       as P
import Control.Arrow (first)
import Control.Monad.Par.Class as PC

import Control.Monad.Par.IO as PIO

-- import qualified Control.Monad.Par.Scheds.TraceDebuggable as TDB
import Control.Monad.State as S

import Control.Concurrent.Chan

--
-- for debugging only:
-- import Debug.Scheduler as P
--
-- import           Control.Parallel    (pseq)
import Data.Dynamic2
import Data.List as List
import Data.Maybe
import Data.Set as Set hiding (map)
import Data.StateElement
import Data.Void

import Control.DeepSeq (deepseq)

import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

-- type SFM s b = State s b
type SFM s b = StateT s IO b

type SF s a b = a -> SFM s b

-- runSF :: SFM s b -> s -> (b,s)
-- runSF = runState
runSF :: SFM s b -> s -> IO (b, s)
runSF = runStateT

-- data OhuaM m globalState result = OhuaM {
--                               moveStateForward :: globalState -> m globalState,
--                               runOhua :: globalState -> m (result, globalState)
--                              }
-- this existential quantification essentially hides the types for ivar and m.
-- this forces somebody with a variable of that type to apply it only to a predefined
-- function that knows what the type of 'ivar' and 'm' is.
-- this way, the types are entirely hidden inside that module and restrict the user/caller
-- to a very specific function, i.e., runOhua and moveStateForward.
-- I love that!
-- sources: https://prime.haskell.org/wiki/ExistentialQuantification
--          https://stackoverflow.com/questions/12031878/what-is-the-purpose-of-rank2types#12033549
-- data OhuaM state result = forall ivar m. (ParIVar ivar m)
--                         => OhuaM {
--                              moveStateForward :: GlobalState ivar state -> m (GlobalState ivar state),
--                              runOhua :: GlobalState ivar state -> m (result, GlobalState ivar state)
--                             }
-- when the data constructor OhuaM is called then the type variables are
-- captured with the according types. when the according functions are called later on, then
-- the input to that call must match the captured types now.
-- the above version quantifies over the whole creation of the data type. it becomes:
-- forall ivar m. (ParIVar ivar m) => ((GlobalState ivar state) -> m (GlobalState ivar state))
--                                 -> ((GlobalState ivar state) -> m (result, GlobalState ivar state))
--                                 -> OhuaM state result
-- but we want to have Rank2Types instead to hide ivar and m! (see the example below!)
data OhuaM result = OhuaM
  { moveStateForward :: forall ivar m. (ParIVar ivar m, MonadIO m) =>
                                         GlobalState ivar -> m (GlobalState ivar)
  , runOhua :: forall ivar m. ( ParIVar ivar m
                              , MonadIO m
                              , NFData (ivar S) -- FIXME giving the MonadIO constraint here seems weird to me because then it totally breaks the abstraction and could write ParIO directly.
                              ) =>
                                GlobalState ivar -> m (result, GlobalState ivar)
  }

-- Example: ExistentialQuantification vs Rank2Types
-- Prelude> set: -XExistentialQuantification
-- Prelude> data T s r = forall ivar m. (Show ivar, Monad m) => TR { f :: (s,ivar) -> m (ivar,s) }
-- Prelude> :t TR
-- TR :: (Monad m, Show ivar) => ((s, ivar) -> m (ivar, s)) -> T s r
-- that is:
-- TR :: forall ivar m. (Monad m, Show ivar) => ((s, ivar) -> m (ivar, s)) -> T s r
-- BUT:
-- Prelude> set: -Rank2Types
-- Prelude> data T s r = TR { f :: forall ivar m. (Show ivar, Monad m) => (s,ivar) -> m (ivar,s) }
-- translates to:
-- Prelude> :t TR
-- TR :: (forall ivar (m :: * -> *). (Show ivar, Monad m) => (s, ivar) -> m (ivar, s)) -> T s r
--
-- ExistentialQuantification makes only sense when we quantify over the output of a function (i.e.)
-- the type of a record. that is because each function captures its own type variable so you can not
-- compose such data as I tried in <*> or =<< with GlobalState (which came from another data).
data GlobalState ivar = GlobalState
  { input :: [ivar S]
  , result :: [ivar S]
  } deriving (Generic)

-- data GlobalState ivar = GlobalState [ivar S] [ivar S] deriving (Generic)
instance (NFData (ivar S)) => NFData (GlobalState ivar)

--
-- shortcoming: this monad implementation is strict because bind requests the
--              actual value. consider the following situation:
--              do
--                 x1 <- a 5
--                 x2 <- b 5
--                 x3 <- c 5
--              this monad will run these 3 statements in sequence because bind
--              always wants the concrete value although it may not actually be
--              used by the directly following computation. to circumvent this
--              case, one would have to use an applicative here:
--              do
--                (x1,x2,x3) <- (,,) <$> a 5 <*> a 5 <*> a 5
--
instance Functor OhuaM where
  fmap f g = OhuaM (moveStateForward g) $ fmap (first f) . runOhua g

instance Applicative OhuaM where
  pure = return
  -- TODO (<*>) = Control.Monad.ap  this is a requirement if the functor is also a monad.
  -- this is the case so we should create a new functor that is not a monad but only an applicative.
  -- in order to do so we need to provide a OhuaM computation in the new applicative functor that
  -- can be ready executed via runOhua! - (Haxl doesn't care)
  (<*>) :: forall a b. OhuaM (a -> b) -> OhuaM a -> OhuaM b
  f <*> a = OhuaM moveState comp
    where
      moveState ::
           forall ivar m. (ParIVar ivar m, MonadIO m)
        => GlobalState ivar
        -> m (GlobalState ivar)
      moveState gs
        -- there is really no computation here, so no need to spawn anything
       = do
        gs' <- moveStateForward a gs
        moveStateForward f gs'
        -- there is no state change here really. I could have returned gs' as well, I suppose.
      comp ::
           forall ivar m. (ParIVar ivar m, MonadIO m, NFData (ivar S))
        => GlobalState ivar
        -> m (b, GlobalState ivar)
      comp gs
        -- run the action first. in the final monad code for OhuaM, the outermost <*>
        -- will execute first. as a result of this code, we will recursively go and
        -- spawn the tasks for the arguments which can happily execute in parallel
        -- until we reach the bottom of the recursion, i.e., the pure function.
        -- then the recursion unwinds again gathering all the results.
       = do
        aVar <- PC.spawn_ $ runOhua a gs -- TODO force evaluation here
        -- run the function
        (fResult, _) <- runOhua f gs
        -- wrap it up by applying the function to the result of the action
        (r, gs') <- PC.get aVar
        return (fResult r, gs')
  -- mf@(OhuaM _) <*> mv@(OhuaM _) = Collected mf [mv]
  -- mf@(OhuaM _) <*> (Collected pf sfs) = Collected mf (pf : sfs)
  -- (Collected pf sfs) <*> mv@(OhuaM sf) = Collected pf sfs ++ [mv]
  -- (Collected pf1 sfs1) <*> (Collected pf2 sfs2) = Collected pf1 (sfs1 ++ (pf2:sfs2))
  --  -- this collecting is only stopped by the monadic bind operator!

instance Monad OhuaM
  --{-# NOINLINE return #-}
                            where
  return :: forall a. a -> OhuaM a
  return v = OhuaM return $ \s -> return (v, s)
  {-# NOINLINE (>>=) #-}
  (>>=) :: HasCallStack => OhuaM a -> (a -> OhuaM b) -> OhuaM b
  f >>= g = OhuaM moveState comp
    where
      moveState ::
           forall ivar m. (ParIVar ivar m, MonadIO m, HasCallStack)
        => GlobalState ivar
        -> m (GlobalState ivar)
      moveState gs = do
        gs' <- moveStateForward f gs
        flip moveStateForward gs' $
          g $
          error "Invariant broken: Don't touch me, state forward moving code!"
      -- comp ::
      --      forall ivar m. (ParIVar ivar m, MonadIO m, NFData (ivar S))
      --   => GlobalState ivar
      --   -> m (b, GlobalState ivar)
      comp gs
          -- there is no need to spawn here!
          -- pipeline parallelism is solely created by smap.
          -- task-level parallelism is solely created by <*>
       = do
        (result0, gs') <- runOhua f gs
        (result1, gs'') <- runOhua (g result0) gs'
        return (result1, gs'')
      {-# INLINE comp #-}

instance MonadIO OhuaM where
  liftIO :: IO a -> OhuaM a
  liftIO ioAction = OhuaM return $ \s -> (, s) <$> liftIO ioAction
  {-# INLINE liftIO #-}

--{-# NOINLINE liftWithIndex #-}
{-# INLINE liftWithIndex #-}
liftWithIndex ::
     (NFData a, Show a, NFData s, Typeable s) => Int -> SF s a b -> a -> OhuaM b
liftWithIndex = liftWithIndexS

--liftWithIndex i f d = liftWithIndex' i $ f d
liftWithIndexS ::
     forall a s b. (Show a, NFData s, Typeable s, NFData a)
  => Int
  -> SF s a b
  -> a
  -> OhuaM b
liftWithIndexS i f d =
  OhuaM (moveState d) (compAndMoveState $ f d)
  where
    compAndMoveState ::
         forall ivar m a. (ParIVar ivar m, MonadIO m)
      => SFM s a
      -> GlobalState ivar
      -> m (a, GlobalState ivar)
    compAndMoveState sf (GlobalState gsIn gsOut)
      -- we define the proper order on the private state right here!
     = do
      let ithIn = gsIn !! i
          ithOut = gsOut !! i
      -- if we do not deepseq here then a previous parallel stage will get
      -- serialized at this point because the monadic operation will always
      -- be evaluated first and then the computation that computes the input
      -- for this algo.
      d `deepseq` pure ()
      localState <- getState ithIn -- this synchronizes access to the local state
      (d', localState') <- liftIO $ runSF sf $ fromS localState
      release ithOut $ toS localState'
      return (d', GlobalState gsIn gsOut)

    moveState :: 
        forall ivar m a. (ParIVar ivar m, MonadIO m)
      => a 
      -> GlobalState ivar 
      -> m (GlobalState ivar)
    moveState token (GlobalState gsIn gsOut) = do
      let ithIn = gsIn !! i
          ithOut = gsOut !! i
      localState <- getState ithIn
      (_, localState') <- return (d, localState) -- id
      release ithOut localState'
      return $ GlobalState gsIn gsOut

    idSf :: SFM s ()
    idSf = return ()
    {-# INLINE idSf #-}

{-# INLINE liftWithIndex' #-}
liftWithIndex' ::
     forall s b. (NFData s, Typeable s)
  => Int
  -> SFM s b
  -> OhuaM b
liftWithIndex' i comp =
  OhuaM (fmap snd . compAndMoveState idSf) (compAndMoveState comp)
  where
    compAndMoveState ::
         forall ivar m a. (ParIVar ivar m, MonadIO m)
      => SFM s a
      -> GlobalState ivar
      -> m (a, GlobalState ivar)
    compAndMoveState sf (GlobalState gsIn gsOut)
      -- we define the proper order on the private state right here!
     = do
      let ithIn = gsIn !! i
          ithOut = gsOut !! i
      localState <- getState ithIn -- this synchronizes access to the local state
      (d', localState') <- liftIO $ runSF sf $ fromS localState
      release ithOut $ toS localState'
      return (d', GlobalState gsIn gsOut)
    idSf :: SFM s ()
    idSf = return ()
    {-# INLINE idSf #-}

--{-# NOINLINE release #-}
release :: (NFData s, ParIVar ivar m) => ivar s -> s -> m ()
release = updateState

{-# INLINE release #-}
{-# INLINE updateState #-}
{-# INLINE getState #-}
updateState :: (NFData s, ParIVar ivar m) => ivar s -> s -> m ()
updateState = PC.put

getState :: (ParFuture ivar m) => ivar s -> m s
getState = PC.get -- will wait for the value

runOhuaM :: (NFData a) => OhuaM a -> [S] -> IO (a, [S])
runOhuaM comp initialState =
  runParIO $
  -- for debugging the scheduler
  -- TDB.runParIO $ do
   do
    inState <- mapM PC.newFull initialState
    outState <- forM initialState $ const PC.new
    (result, _) <- runOhua comp $ GlobalState inState outState
    finalState <- mapM getState outState
    return (result, finalState)
-- envisioned API:
--
-- s1 = liftWithIndex 5 $ \ x -> ....
-- OhuaM ..
-- do
--   r0 <- a x
--   r1 <- b x
--   r2 <- c x
--   xs <- d r2
--   <- smap c xs
--
--   where c x = do
--                r01 <- e x
--                r02 <- f r01
--                return r02
--
-- runOhua m s
