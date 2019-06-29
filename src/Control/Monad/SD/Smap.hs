{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

module Control.Monad.SD.Smap
    ( smap
    , smapGen
    ) where

import Control.Monad
import Control.Monad.Generator
import Control.Monad.IO.Class
import Control.Monad.Par.Class as PC
import Control.Monad.SD.Ohua
import Data.StateElement

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
    case xs of
        [] -> OhuaM moveState (fmap (([] :: [b]), ) . moveState) -- if no data was given then just move the state.
        _ -> OhuaM moveState comp
    -- all we need to do is to move the state once, no need to do it for each
    -- of the elements in the array!
  where
    moveState ::
           forall ivar m. (ParIVar ivar m, MonadIO m)
        => GlobalState ivar
        -> m (GlobalState ivar)
    moveState = moveStateForward $ algo (error "I do not want to be touched!")
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
                [] -> error "I should be unreachable"
                [y] -> pure <$> spawnComp y originalOut
                (y:ys) -> do
                    stateVec <- newEmptyStateVec
                    (:) <$> spawnComp y stateVec <*> go stateVec ys
          where
            spawnComp e stateVec =
                PC.spawn $ runOhua (f e) $ GlobalState prevState stateVec

type AlgoRunner m ivar t result
     --(ParIVar ivar m, MonadIO m, MonadIO ivar) =>
     = t -> [ivar S] -> [ivar S] -> m (ivar (result, GlobalState ivar))

type PipelineStrategy a b
     = forall m ivar. (ParIVar ivar m, MonadIO m) =>
                          AlgoRunner m ivar a b -- algo runner
                           -> Int -- state vector size
                               -> [ivar S] -- final state vector
                                   -> [ivar S] -- current state vector
                                       -> Generator IO a -> a -> m [ivar ( b
                                                                         , GlobalState ivar)]

-- TODO: Check if this can deal with empty generators. Furthermore
-- it always advances the generator one position more than what it
-- currently processes to find the end of the generator before the
-- last item is processed so that it can spawn that computation with
-- the original output state vector.
smapGen ::
       forall a b. (NFData b, Show a)
    => (a -> OhuaM b)
    -> Generator IO a
    -> OhuaM [b]
#ifdef UNTHROTTLED
smapGen = smapGenInternal unthrottledPipe
#else
smapGen = smapGenInternal throttledPipe
#endif
smapGenInternal ::
       forall a b. (NFData b, Show a)
    => PipelineStrategy a b
    -> (a -> OhuaM b)
    -> Generator IO a
    -> OhuaM [b]
smapGenInternal pipelineStrategy algo gen =
    OhuaM moveState $ \g@(GlobalState gsIn gsOut) ->
        liftIO (step gen) >>= \case
            Nothing -> fmap (([] :: [b]), ) $ moveState g
            Just (a, gen') -> do
                futures <- spawnFutures gsOut gsIn gen' a
                values <- mapM PC.get futures
                pure (map fst values, GlobalState gsIn gsOut)
  where
    spawnFutures lastStateOut = pipelineStrategy runAlgo stateVSize lastStateOut
      where
        stateVSize = length lastStateOut
        runAlgo e stateIn stateOut =
            PC.spawn $ runOhua (algo e) $ GlobalState stateIn stateOut
    moveState ::
           forall ivar m. (ParIVar ivar m, MonadIO m)
        => GlobalState ivar
        -> m (GlobalState ivar)
    moveState = moveStateForward $ algo (undefined :: a)

newEmptyStateVec size = sequence $ replicate size PC.new

unthrottledPipe :: PipelineStrategy a b
unthrottledPipe runAlgo stateVSize lastStateOut stateIn gen' a =
    liftIO (step gen') >>= \case
        Nothing -> pure <$> runLastAlgo
        Just (a', gen'') -> do
            newStateVec <- newEmptyStateVec stateVSize
      -- the parallelism is in the applicative.
      -- runAlgo immediately returns and gives me an IVar.
      -- go is the recursion.
      -- I need to change `go` to take the current list of IVars.
      -- Then a simple version of throttling becomes totally easy.
      -- I just need to check the length of the list and once it has
      -- reached the predefined threshold, I need to stop and wait for
      -- the IVar at the head of the list before contiuing to spawn.
      -- (This assumes that the head is the one finishing first.)
            (:) <$> runAlgo a stateIn newStateVec <*>
                unthrottledPipe
                    runAlgo
                    stateVSize
                    lastStateOut
                    newStateVec
                    gen''
                    a'
  where
    runLastAlgo = runAlgo a stateIn lastStateOut

limit :: Int
limit = 10

throttledPipe :: PipelineStrategy a b
throttledPipe runAlgo stateVSize lastStateOut stateIn gen a
    -- 1. get the first n
 = do
    (genLimited, a', lastLimitOut, firstResults) <-
        unthrottled limit [] stateIn gen a
    -- 2. get on head of results before spawning a new computation
    throttled firstResults 0 lastLimitOut genLimited a'
    -- unthrottled ::
    --      Int
    --   -> [ivar (b, GlobalState ivar)]
    --   -> [ivar S]
    --   -> [ivar S]
    --   -> Generator IO a
    --   -> a
    --   -> m (Generator IO a, a, [ivar S], [ivar (b, GlobalState ivar)])
  where
    unthrottled l results sIn gen' a' = do
        if l == 0
            then return (gen', a', sIn, results)
            else do
                liftIO (step gen') >>= \case
                    Nothing -> do
                        res <- runAlgo a' sIn lastStateOut
              -- from now on the generator always returns NOTHING, so it is
              -- ok to use it as the state input vector to the next iteration.
                        return (gen', a', lastStateOut, results ++ [res])
                    Just (a'', gen'') -> do
                        newStateVec <- newEmptyStateVec stateVSize
                        resultFuture <- runAlgo a' sIn newStateVec
                        unthrottled
                            (l - 1)
                            (results ++ [resultFuture])
                            newStateVec
                            gen''
                            a''
    -- throttled ::
      --    [ivar (b, GlobalState ivar)]
      -- -> Int
      -- -> [ivar S]
      -- -> Generator IO a
      -- -> a
      -- -> m [ivar (b, GlobalState ivar)]
    throttled results lastPending sIn gen' a' = do
        _ <- PC.get $ results !! lastPending -- throttling
        liftIO (step gen') >>= \case
            Nothing -> do
                res <- runAlgo a' sIn lastStateOut
                return $ results ++ [res]
            Just (a'', gen'') -> do
                newStateVec <- newEmptyStateVec stateVSize
                ivar <- runAlgo a' sIn newStateVec
                throttled
                    (results ++ [ivar])
                    (lastPending + 1)
                    newStateVec
                    gen''
                    a''
