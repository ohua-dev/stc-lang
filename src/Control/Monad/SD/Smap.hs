{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.SD.Smap
  ( smap
  , smapGen
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Par.Class as PC
import Control.Monad.SD.Ohua
import Data.StateElement
import Monad.Generator

-- FIXME this should be based on smapGen!
-- this spawns the computations for the elements but integrates the
-- state dependencies!
-- version used for debugging:
-- smap :: (NFData b, NFData s, Show a, ParIVar ivar m, NFData (ivar s)) => (Int -> a -> OhuaM m (GlobalState ivar s) b) -> [a] -> OhuaM m (GlobalState ivar s) [b]
--{-# NOINLINE smap #-}
--{-# INLINE smap #-}
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
-- unthrottledPipe :: () => [ivar S] -> Generator IO a -> [ivar b]
-- unthrottledPipe stateIn gen' a =
--   liftIO (step gen') >>= \case
--     Nothing -> pure <$> runLastAlgo
--     Just (a', gen'') -> do
--       newStateVec <- newEmptyStateVec
--       -- the parallelism is in the applicative.
--       -- runAlgo immediately returns and gives me an IVar.
--       -- go is the recursion.
--       -- I need to change `go` to take the current list of IVars.
--       -- Then a simple version of throttling becomes totally easy.
--       -- I just need to check the length of the list and once it has
--       -- reached the predefined threshold, I need to stop and wait for
--       -- the IVar at the head of the list before contiuing to spawn.
--       -- (This assumes that the head is the one finishing first.)
--       (:) <$> runAlgo a stateIn newStateVec <*>
--         unthrottledPipe newStateVec gen'' a'
--   where
--     runLastAlgo = runAlgo a stateIn lastStateOut
