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
{-# LANGUAGE NamedFieldPuns #-}

--- this implementation does not rely on channels. it builds on futures!
module Monad.FuturesBasedMonad
  ( smap
  , smapGen
  , case_
  , liftWithIndex
  , liftWithIndex'
  , SF
  , SFM
  , runOhuaM
  , OhuaM
  , runSTCLang
  , liftWithState
  , smapSTC
  , liftSignal
  , runSignals
  , filterSignal
  , parMapReduceRangeThresh
  ) where

import Control.Monad

-- import           Control.Monad.Par       as P
import Control.Arrow (first)
import Control.Monad.Par.Class as PC
import Control.Monad.Par.Combinator (InclusiveRange, InclusiveRange(..))
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

-- import           Debug.Trace
import GHC.Generics (Generic)

-- import           Control.DeepSeq
import Monad.Generator

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

instance Monad OhuaM where
  {-# NOINLINE return #-}
  return :: forall a. a -> OhuaM a
  return v = OhuaM return $ \s -> return (v, s)
  {-# NOINLINE (>>=) #-}
  (>>=) :: forall a b. OhuaM a -> (a -> OhuaM b) -> OhuaM b
  f >>= g = OhuaM moveState comp
    where
      moveState ::
           forall ivar m. (ParIVar ivar m, MonadIO m)
        => GlobalState ivar
        -> m (GlobalState ivar)
      moveState gs = do
        gs' <- moveStateForward f gs
        flip moveStateForward gs' $
          g $
          error "Invariant broken: Don't touch me, state forward moving code!"
      comp ::
           forall ivar m. (ParIVar ivar m, MonadIO m, NFData (ivar S))
        => GlobalState ivar
        -> m (b, GlobalState ivar)
      comp gs
          -- there is no need to spawn here!
          -- pipeline parallelism is solely created by smap.
          -- task-level parallelism is solely created by <*>
       = do
        (result0, gs') <- runOhua f gs
        (result1, gs'') <- runOhua (g result0) gs'
        return (result1, gs'')

instance MonadIO OhuaM where
  liftIO :: IO a -> OhuaM a
  liftIO ioAction = OhuaM return $ \s -> (, s) <$> liftIO ioAction

{-# NOINLINE liftWithIndex #-}
liftWithIndex ::
     (Show a, NFData s, Typeable s) => Int -> SF s a b -> a -> OhuaM b
liftWithIndex i f d = liftWithIndex' i $ f d

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

{-# NOINLINE release #-}
release :: (NFData s, ParIVar ivar m) => ivar s -> s -> m ()
release = updateState

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

-- this spawns the computations for the elements but integrates the
-- state dependencies!
-- version used for debugging:
-- smap :: (NFData b, NFData s, Show a, ParIVar ivar m, NFData (ivar s)) => (Int -> a -> OhuaM m (GlobalState ivar s) b) -> [a] -> OhuaM m (GlobalState ivar s) [b]
{-# NOINLINE smap #-}
smap ::
     forall a b. (NFData b, Show a)
  => (a -> OhuaM b)
  -> [a]
  -> OhuaM [b]
smap algo xs =
  case xs
        -- event thought the first case here indicates that this
        -- function can handle empty lists it actually
        -- can't. Something with the move state function is not
        -- correct.
        of
    [] -> OhuaM moveState (fmap (([] :: [b]), ) . moveState) -- if no data was given then just move the state.
    _ -> OhuaM moveState comp
    -- all we need to do is to move the state once, no need to do it for each
    -- of the elements in the array!
  where
    moveState ::
         forall ivar m. (ParIVar ivar m, MonadIO m)
      => GlobalState ivar
      -> m (GlobalState ivar)
    moveState = moveStateForward $ algo undefined
    comp ::
         forall ivar m. (ParIVar ivar m, MonadIO m, NFData (ivar S))
      => GlobalState ivar
      -> m ([b], GlobalState ivar)
    comp (GlobalState gsIn gsOut) = do
      futures <- smap' algo gsOut gsIn xs
      results <- forM futures PC.get -- collect the results
      let result = map fst results
      return (result, GlobalState gsIn gsOut)
    -- This function replicates the state as many times as their are values in
    -- the list and spawns the computation.
    smap' ::
         (NFData b, Show a, ParIVar ivar m, MonadIO m, NFData (ivar S))
      => (a -> OhuaM b)
      -> [ivar S]
      -> [ivar S]
      -> [a]
      -> m [ivar (b, GlobalState ivar)]
    smap' f originalOut initialState = go initialState
      where
        newEmptyStateVec = sequence $ replicate stateVSize PC.new -- create the new output state
        stateVSize = length initialState
        go prevState l =
          case l of
            [] -> pure []
            [y] -> pure <$> spawnComp y originalOut
            (y:ys) -> do
              stateVec <- newEmptyStateVec
              (:) <$> spawnComp y stateVec <*> go stateVec ys
          where
            spawnComp e stateVec =
              PC.spawn $ runOhua (f e) $ GlobalState prevState stateVec

-- Again like smap this cannot deal with empty generators. Furthermore
-- it always advances the generator one position more than what it
-- currently processes to find the end of the generator before the
-- last item is processed so that it can spawn that computation with
-- the original output state vector.
smapGen ::
     forall a b. (NFData b, Show a)
  => (a -> OhuaM b)
  -> Generator IO a
  -> OhuaM [b]
smapGen algo gen =
  OhuaM moveState $ \g@(GlobalState gsIn gsOut) ->
    liftIO (step gen) >>= \case
      Nothing -> fmap (([] :: [b]), ) $ moveState g
      Just (a, gen') -> do
        futures <- spawnFutures gsOut gsIn gen' a
        values <- mapM PC.get futures
        pure (map fst values, GlobalState gsIn gsOut)
  where
    spawnFutures lastStateOut = go
      where
        newEmptyStateVec = sequence $ replicate stateVSize PC.new -- create the new output state
        stateVSize = length lastStateOut
        runAlgo e stateIn stateOut =
          PC.spawn $ runOhua (algo e) $ GlobalState stateIn stateOut
        go stateIn gen' a =
          liftIO (step gen') >>= \case
            Nothing -> pure <$> runLastAlgo
            Just (a', gen'') -> do
              newStateVec <- newEmptyStateVec
              (:) <$> runAlgo a stateIn newStateVec <*> go newStateVec gen'' a'
          where
            runLastAlgo = runAlgo a stateIn lastStateOut
    moveState ::
         forall ivar m. (ParIVar ivar m, MonadIO m)
      => GlobalState ivar
      -> m (GlobalState ivar)
    moveState = moveStateForward $ algo (undefined :: a)

case_ ::
     forall a p. (NFData a, Show a, Eq p)
  => p
  -> [(p, OhuaM a)]
  -> OhuaM a
case_ cond patternsAndBranches = OhuaM moveState comp
  where
    moveState ::
         forall ivar m. (ParIVar ivar m, MonadIO m)
      => GlobalState ivar
      -> m (GlobalState ivar)
    moveState gs =
      (foldM (flip moveStateForward) gs . map snd) patternsAndBranches
    comp ::
         forall ivar m. (ParIVar ivar m, Monad m, MonadIO m, NFData (ivar S))
      => GlobalState ivar
      -> m (a, GlobalState ivar)
    comp gs
      -- find the first pattern that matches
     = do
      let idx = List.findIndex ((cond ==) . fst) patternsAndBranches
      let ith = fromMaybe (error "No pattern found for condition.") idx
      -- one could of course do the following in parallel but it is not a performance bottleneck as of now.
      let trueBranch = patternsAndBranches !! ith
      let falseBranches =
            ((\(before, _:after) -> before ++ after) . List.splitAt ith)
              patternsAndBranches
      gs' <- foldM (flip moveStateForward) gs $ map snd falseBranches
      (result, gs'') <- runOhua (snd trueBranch) gs'
      return (result, gs'')

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
data CollSt = CollSt
  { states :: [S]
  , signals :: [IO S]
  }

instance Monoid CollSt where
  mempty = CollSt [] []
  CollSt st1 si1 `mappend` CollSt st2 si2 =
    CollSt (st1 `mappend` st2) (si1 `mappend` si2)

type STCLang a b = StateT CollSt IO (a -> OhuaM b)

liftWithState ::
     (Typeable s, NFData a, NFData s, Show a)
  => IO s
  -> (a -> StateT s IO b)
  -> STCLang a b
liftWithState state stateThread = do
  s0 <- lift state
  l <- S.state $ \s -> (length $ states s, s {states = states s ++ [toS s0]})
  pure $ liftWithIndex l stateThread

runSTCLang :: (NFData a, NFData b) => STCLang a b -> a -> IO (b, [S])
runSTCLang langComp a = do
  (comp, gs) <- S.runStateT langComp mempty
  runOhuaM (comp a) $ states gs

smapSTC ::
     forall a b. (NFData b, Show a)
  => STCLang a b
  -> STCLang [a] [b]
smapSTC comp = smap <$> comp

type Signal = IO

type Signals = (Int, S)

instance Show S where
  show _ = "S"

liftSignal :: (Typeable a, NFData a) => Signal a -> IO a -> STCLang Signals a
liftSignal s0 init = do
  idx <-
    S.state $ \s@CollSt {signals} ->
      (length signals, s {signals = signals ++ [toS <$> s0]})
  liftWithState init $ \(i, s) ->
    if i == idx
      then do
        let my = fromS s
        S.put my
        pure my
      else S.get

runSignals :: NFData a => STCLang Signals a -> IO ([a], [S])
runSignals comp = do
  (comp', s) <- S.runStateT comp mempty
  chan <- newChan
  forM_ (zip [0 ..] $ signals s) $ \(idx, sig) ->
    forever $ do
      event <- sig
      writeChan chan $ Just (idx, event)
  let signalGen = chanToGenerator chan
  runOhuaM (smapGen comp' signalGen) $ states s

-- | @filter init p f@ applies @f@ to only those values @a@ that satisfy the
-- predicate @p@. For values not satisfying it returns the last computed value
-- (initially @init@)
filterSignal ::
     (Show b, Typeable b, NFData b, NFData a)
  => IO b -- Initial value for the output
  -> (a -> OhuaM Bool) -- predicate
  -> (a -> OhuaM b) -- computation to perform on `a`
  -> STCLang a b
filterSignal init cond f = do
  g <- liftWithState init $ maybe S.get (\i -> S.put i >> pure i)
  return $ \item -> do
    r <- cond item
    i <- case_ r [(True, Just <$> f item), (False, pure Nothing)]
    g i

----
-- The below comes originally from: https://hackage.haskell.org/package/monad-par-extras-0.3.3/docs/src/Control-Monad-Par-Combinator.html#parMapReduceRangeThresh
----
-- | Computes a binary map\/reduce over a finite range.  The range is
-- recursively split into two, the result for each half is computed in
-- parallel, and then the two results are combined.  When the range
-- reaches the threshold size, the remaining elements of the range are
-- computed sequentially.
--
-- For example, the following is a parallel implementation of
--
-- >  foldl (+) 0 (map (^2) [1..10^6])
--
-- > parMapReduceRangeThresh 100 (InclusiveRange 1 (10^6))
-- >        (\x -> return (x^2))
-- >        (\x y -> return (x+y))
-- >        0
--
-- parMapReduceRangeThresh ::
--      (NFData a, ParFuture iv p)
--   => Int -- ^ threshold
--   -> InclusiveRange -- ^ range over which to calculate
--   -> (Int -> p a) -- ^ compute one result
--   -> (a -> a -> p a) -- ^ combine two results (associative)
--   -> a -- ^ initial result
--   -> p a
-- parMapReduceRangeThresh threshold range fn binop init =
-- loop min max
-- where
--   loop min max
--     | max - min <= threshold =
--       let mapred a b = do
--             x <- fn b
--             result <- a `binop` x
--             return result
--        in foldM mapred init [min .. max]
--     | otherwise = do
--       let mid = min + ((max - min) `quot` 2)
--       rght <- spawn $ loop (mid + 1) max
--       l <- loop min mid
--       r <- get rght
--       l `binop` r
instance Show InclusiveRange

parMapReduceRangeThresh ::
     (NFData a, Typeable a, Show a)
  => Int -- ^ threshold
  -> InclusiveRange -- ^ range over which to calculate
  -> (Int -> a) -- ^ compute one result
  -> (a -> a -> a) -- ^ combine two results (associative)
  -> a -- ^ initial result
  -> IO a
parMapReduceRangeThresh threshold range fn binop init
  -- sadly I could not use STCLang to build this :(
  -- reason: it must be STCLang a b to implement liftSignal instead of just
  -- STCLang b just like OhuaM b
 = do
  (_, [reduceState]) <- runOhuaM mapReduce [toS init]
  return $ fromS reduceState
  where
    mapReduce = do
      smapGen
        ((pure . mapAndCombine) >=> (liftWithIndex 0 reduce))
        chunkGenerator
    chunkGenerator =
      flip stateToGenerator range $ do
        (InclusiveRange mi ma) <- S.get
        if mi >= ma
          then return Nothing
          else let mi' = min (mi + threshold) ma
                in do S.put $ InclusiveRange (mi' + 1) ma
                      return $ Just $ InclusiveRange mi mi'
    mapAndCombine (InclusiveRange mi ma) =
      let mapred a b =
            let x = fn b
                result = a `binop` x
             in result
       in List.foldl mapred init [mi .. ma]
    reduce v = S.get >>= (S.put . (`binop` v))
