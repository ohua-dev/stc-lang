{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

--- this implementation does not rely on channels. it builds on futures!


module FuturesBasedMonad where

import           Control.Monad
-- import           Control.Monad.Par       as P
import           Control.Monad.Par.IO    as PIO
import           Control.Monad.Par.Class as PC
import           Control.Monad.State     as S
import           Control.Arrow (first)
--
-- for debugging only:
-- import Scheduler as P
-- import Control.DeepSeq
--
-- import           Control.Parallel    (pseq)
import           Data.Set               as Set hiding (map)
-- import           Debug.Trace
import           GHC.Generics                   (Generic)
-- import           System.IO.Unsafe

-- type SFM s b = State s b
type SFM s b = StateT s IO b

type SF s a b = a -> SFM s b

-- runSF :: SFM s b -> s -> (b,s)
-- runSF = runState
runSF :: SFM s b -> s -> IO (b,s)
runSF = runStateT

newtype OhuaM m s a = OhuaM { runOhua :: s -> m ( a -- the result
                                                 , s -- the global state
                                                 )}

data GlobalState ivar s = GlobalState [ivar s] [ivar s] (Set.Set Int) deriving (Generic)
instance (NFData s, NFData (ivar s)) => NFData (GlobalState ivar s)

-- logOhuaM :: String -> OhuaM s ()
-- logOhuaM msg = OhuaM $ \s -> unsafePerformIO (putStrLn msg) `seq` return ((), s)


-- {-# NOINLINE ohuaPrint #-}
-- ohuaPrint :: Show a => b -> a -> b
-- ohuaPrint c msg =
--   unsafePerformIO $ print msg >> return c
--
-- {-# NOINLINE oPrint #-}
-- oPrint :: Show a => a -> ()
-- oPrint msg = unsafePerformIO $ print msg

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
instance (Functor m) => Functor (OhuaM m s) where
  fmap f (OhuaM g) = OhuaM $ \s -> first f <$> g s

instance (NFData s, ParIVar ivar m) => Applicative (OhuaM m s) where
  pure = return
  -- TODO (<*>) = Control.Monad.ap  this is a requirement if the functor is also a monad.
  -- this is the case so we should create a new functor that is not a monad but only an applicative.
  -- in order to do so we need to provide a OhuaM computation in the new applicative functor that
  -- can be ready executed via runOhua! - (Haxl doesn't care)
  (<*>) :: forall a b.OhuaM m s (a -> b) -> OhuaM m s a -> OhuaM m s b
  f <*> a = OhuaM comp
    where
      comp :: () => s -> m (b,s)
      comp gs = do
        -- run the action first. in the final monad code for OhuaM, the outermost <*>
        -- will execute first. as a result of this code, we will recursively go and
        -- spawn the tasks for the arguments which can happily execute in parallel
        -- until we reach the bottom of the recursion, i.e., the pure function.
        -- then the recursion unwinds again gathering all the results.
        aVar <- PC.spawn_ $ runOhua a gs -- TODO force evaluation here

        -- run the function
        (fResult, _) <- runOhua f gs

        -- wrap it up by applying the function to the result of the action
        (r,gs') <- PC.get aVar
        return (fResult r,gs')


  -- mf@(OhuaM _) <*> mv@(OhuaM _) = Collected mf [mv]
  -- mf@(OhuaM _) <*> (Collected pf sfs) = Collected mf (pf : sfs)
  -- (Collected pf sfs) <*> mv@(OhuaM sf) = Collected pf sfs ++ [mv]
  -- (Collected pf1 sfs1) <*> (Collected pf2 sfs2) = Collected pf1 (sfs1 ++ (pf2:sfs2))
  --  -- this collecting is only stopped by the monadic bind operator!

instance (NFData s, ParIVar ivar m) => Monad (OhuaM m s) where
  {-# NOINLINE return #-}
  return :: forall a.a -> OhuaM m s a
  return v = OhuaM $ \s -> return (v, s)

  {-# NOINLINE (>>=) #-}
  (>>=) :: forall a b.OhuaM m s a -> (a -> OhuaM m s b) -> OhuaM m s b
  f >>= g = OhuaM comp
      where
        comp :: s -> m (b, s)
        comp gs = do
          -- there is no need to spawn here!
          -- pipeline parallelism is solely created by smap.
          -- task-level parallelism is solely created by <*>
          (result0, gs') <- runOhua f gs
          (result1, gs'') <- runOhua (g result0) gs'
          return (result1, gs'')

instance (NFData s, ParIVar ivar m, MonadIO m) => MonadIO (OhuaM m s) where
  liftIO :: IO a -> OhuaM m s a
  liftIO ioAction = OhuaM $ \s -> (,s) <$> liftIO ioAction

liftPar :: (ParIVar ivar m) => m a -> OhuaM m (GlobalState ivar s) a
liftPar p = OhuaM $ \s -> (,s) <$> p

-- version with more input used for debugging:
-- liftWithIndex :: (NFData s, Show a, ParIVar ivar m, NFData (ivar s), MonadIO m) => String -> Int -> SF s a b -> Int -> a -> OhuaM m (GlobalState ivar s) b
-- liftWithIndex _ i f _ d = do
{-# NOINLINE liftWithIndex #-}
liftWithIndex :: (NFData s, Show a, ParIVar ivar m, NFData (ivar s), MonadIO m) => Int -> SF s a b -> a -> OhuaM m (GlobalState ivar s) b
liftWithIndex i f d = liftWithIndex' i $ f d

liftWithIndex' :: (NFData s, ParIVar ivar m, NFData (ivar s), MonadIO m) => Int -> SFM s b -> OhuaM m (GlobalState ivar s) b
liftWithIndex' i comp = do
  -- we define the proper order on the private state right here!
  GlobalState gsIn gsOut touchedState <- oget
  let ithIn = gsIn !! i
      ithOut = gsOut !! i
  localState <- liftPar $ getState ithIn -- this synchronizes access to the local state
  (d', localState') <- liftIO $ runSF comp localState
  liftPar $ release ithOut localState'
  oput $ GlobalState gsIn gsOut $ Set.insert i touchedState
  return d'


{-# NOINLINE release #-}
release :: (NFData s, ParIVar ivar m) => ivar s -> s -> m ()
release = updateState

oget :: (ParIVar ivar m) => OhuaM m s s
oget = OhuaM (\s -> return (s,s))

oput :: (ParIVar ivar m) => s -> OhuaM m s ()
oput newState = OhuaM (const $ return ((), newState))

updateState :: (NFData s, ParIVar ivar m) => ivar s -> s -> m ()
updateState = PC.put

getState :: (ParFuture ivar m) => ivar s -> m s
getState = PC.get -- will wait for the value

runOhuaM :: (NFData a, NFData s) => OhuaM ParIO (GlobalState IVar s) a -> [s] -> IO (a,[s])
runOhuaM comp initialState = PIO.runParIO $ do
  inState <- mapM PC.newFull initialState
  outState <- forM initialState $ const PC.new
  (result, _) <- runOhua comp $ GlobalState inState outState Set.empty
  finalState <- mapM getState outState
  return (result, finalState)

-- this spawns the computations for the elements but integrates the
-- state dependencies!
-- version used for debugging:
-- smap :: (NFData b, NFData s, Show a, ParIVar ivar m, NFData (ivar s)) => (Int -> a -> OhuaM m (GlobalState ivar s) b) -> [a] -> OhuaM m (GlobalState ivar s) [b]
{-# NOINLINE smap #-}
smap :: (NFData b, NFData s, Show a, ParIVar ivar m, NFData (ivar s)) => (a -> OhuaM m (GlobalState ivar s) b) -> [a] -> OhuaM m (GlobalState ivar s) [b]
smap algo xs = do
  (GlobalState gsIn gsOut touched) <- oget -- get me the initial state
  futures <- liftPar $ smap' algo xs [0..] gsIn touched
  results <- liftPar $ forM futures PC.get -- collect the results
  let result = map fst results
  let (GlobalState _ gsOut' touchedSMap) = (last . map snd) results
  -- merge: find the local states touched by the computation f and
  --        move them over to gsOut
  gsOut'' <- liftPar $ merge gsOut gsOut' touchedSMap
  oput $ GlobalState gsIn gsOut'' $ Set.union touched touchedSMap
  return result
  where
    smap' :: (NFData b, NFData s, Show a, ParIVar ivar m, NFData (ivar s)) =>
                        (a -> OhuaM m (GlobalState ivar s) b) ->
                        -- debugging: (Int -> a -> OhuaM m (GlobalState ivar s) b) ->
                        [a] ->
                        [Int] ->
                        [ivar s] ->
                        Set Int ->
                        m [ivar (b, GlobalState ivar s)]
    smap' f (y:ys) (ident:idents) prevState touched = do
      outS <- forM prevState $ const PC.new -- create the new output state
      -- debugging: result <- PC.spawn $ runOhua (f ident y) $ GlobalState prevState outS Set.empty
      result <- PC.spawn $ runOhua (f y) $ GlobalState prevState outS Set.empty
      -- traceM $ "spawned smap computation: " ++ show ident
      rest <- smap' f ys idents outS touched
      return $ result : rest
    smap' _ [] _ _ _ = return []
    merge :: (NFData s, ParIVar ivar m) => [ivar s] -> [ivar s] -> Set Int -> m [ivar s]
    merge initialOut computedOut touchedSMap = do
      updateTouched computedOut initialOut touchedSMap
      return initialOut
    updateTouched :: (NFData s, ParIVar ivar m) => [ivar s] -> [ivar s] -> Set Int -> m ()
    updateTouched from to touched =
      forM_ (Set.toAscList touched) $ \i -> do
          let computedLocalState = from !! i
          let emptyLocalState = to !! i
          result <- PC.get computedLocalState -- is already available!
          PC.put emptyLocalState result



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
