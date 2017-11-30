{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

--- this implementation does not rely on channels. it builds on futures!

module FuturesBasedMonad where

import Control.Monad
import Control.Monad.State as S
-- import Control.Monad.Par as P

import Scheduler as P
import Control.DeepSeq

import GHC.Generics (Generic)
import Debug.Trace
import System.IO.Unsafe
import Data.Set as Set hiding (map)
import Control.Parallel (pseq)

type SFM s b = State s b
-- type SFM s b = StateT s IO b

type SF s a b = a -> SFM s b

runSF :: SFM s b -> s -> (b,s)
runSF = runState
-- runSF = runStateT

newtype OhuaM s a = OhuaM { runOhua :: s -> (Par ( a -- the result
                                                 , s -- the global state
                                                 ))
                                               }
data GlobalState s = GlobalState [IVar s] [IVar s] (Set.Set Int) deriving (Generic)
instance NFData a => NFData (GlobalState a)

logOhuaM :: String -> OhuaM s ()
logOhuaM msg = OhuaM $ \s -> unsafePerformIO (putStrLn msg) `seq` return ((), s)


{-# NOINLINE ohuaPrint #-}
ohuaPrint :: Show a => b -> a -> b
ohuaPrint c msg =
  unsafePerformIO $ (print msg) >> return c

{-# NOINLINE oPrint #-}
oPrint :: Show a => a -> ()
oPrint msg = unsafePerformIO $ print msg

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
instance (NFData s) => Functor (OhuaM s) where
  fmap = Control.Monad.liftM

instance NFData s => Applicative (OhuaM s) where
  pure = return
  (<*>) = Control.Monad.ap -- FIXME implement true task-level parallelism here

instance NFData s => Monad (OhuaM s) where
  {-# NOINLINE return #-}
  return :: forall a.a -> OhuaM s a
  return v = OhuaM comp
      where
        comp :: s -> Par (a, s)
        comp gs = do
          return (v,gs)

  {-# NOINLINE (>>=) #-}
  (>>=) :: forall a b.OhuaM s a -> (a -> OhuaM s b) -> OhuaM s b
  f >>= g = OhuaM comp
      where
        comp :: s -> Par (b, s)
        comp gs = do
          -- there is no need to spawn here!
          -- pipeline parallelism is solely created by smap.
          -- task-level parallelism is solely created by <*>
          (result0, gs') <- runOhua f gs
          (result1, gs'') <- runOhua (g result0) gs'
          return (result1, gs'')

liftPar :: Par a -> OhuaM (GlobalState s) a
liftPar p = OhuaM comp
  where
    comp s = do
      result <- p
      return (result,s)

-- liftWithIndex :: (NFData s) => Int -> SF s a b -> a -> OhuaM (GlobalState s) b
-- liftWithIndex i f d = do

-- {-# NOINLINE liftWithIndex #-}
-- liftWithIndex :: forall s a b.(NFData s, Show a) => String -> Int -> SF s a b -> a -> OhuaM (GlobalState s) b
-- liftWithIndex name i f d = do
--   -- we define the proper order on the private state right here!
--   (GlobalState gsIn gsOut touchedState) <- (logOhuaM $ "running -> " ++ name ++ " -> " ++ show d) >> oget
--   let (_,ithIn:_) = splitAt i gsIn
--   let (_,ithOut:_) = splitAt i gsOut
--   -- traceM $ "waiting on lock ... ->" ++ name
--   ithIn' <- (logOhuaM $ "waiting on lock ... ->" ++ name ++ " -> " ++ show d) >> return ithIn
--   localState <- liftPar $ getState ithIn' -- this synchronizes access to the local state
--   -- traceM $ "lock released! -> " ++ name
--   localState'' <- (oPrint $ "lock acquired! -> " ++ name ++ " -> " ++ show d) `pseq` return localState
--   (d', localState') <- return $ runSF (f d) localState''
--   c <- (d', localState') `pseq` return $ oPrint $ "done with computation -> " ++ name ++ " -> " ++ show d
--   -- (d'',ls') <- (oPrint $ "done with computation -> " ++ name ++ " -> " ++ show d) `pseq` return (d',localState')
--   y <- c `pseq` liftPar $ release ithOut localState' touchedState gsIn gsOut d name
--   x <- y `pseq` (return d')
--   -- let x' = ohuaPrint x $ "after -> " ++ name ++ " -> " ++ show d
--   (ohuaPrint x $ "after -> " ++ name ++ " -> " ++ show d) `pseq` oput $ (GlobalState gsIn gsOut) $ Set.insert i touchedState
--   return x

{-# NOINLINE liftWithIndex #-}
liftWithIndex :: forall s a b.(NFData s, Show a) => String -> Int -> SF s a b -> Int -> a -> OhuaM (GlobalState s) b
liftWithIndex name i f ident d = do
  -- we define the proper order on the private state right here!
  (GlobalState gsIn gsOut touchedState) <- (logOhuaM $ "running -> " ++ name ++ " -> " ++ show ident) >> oget
  let (_,ithIn:_) = splitAt i gsIn
  let (_,ithOut:_) = splitAt i gsOut
  -- traceM $ "waiting on lock ... ->" ++ name
  ithIn' <- (logOhuaM $ "waiting on lock ... ->" ++ name ++ " -> " ++ show ident) >> return ithIn
  localState <- liftPar $ getState ithIn' -- this synchronizes access to the local state
  -- traceM $ "lock released! -> " ++ name
  return $! oPrint $ "lock acquired! -> " ++ name ++ " -> " ++ show ident
  (d', localState') <- return $ runSF (f d) localState
  c <- return $ oPrint $ "done with computation -> " ++ name ++ " -> " ++ show ident
  y <- liftPar $ release ithOut localState' touchedState gsIn gsOut ident name
  (oPrint $ "after -> " ++ name ++ " -> " ++ show ident) `pseq` oput $ (GlobalState gsIn gsOut) $ Set.insert i touchedState
  return d'

{-# NOINLINE release #-}
release :: (NFData s, Show a) => IVar s -> s -> Set Int -> [IVar s] -> [IVar s] -> a -> String -> Par ()
release ithOut0 localState0 touchedState0 gsIn0 gsOut0 d0 name0 = do
  x <- return $ oPrint $ "about to release lock -> " ++ name0 ++ " -> " ++ show d0
  y <- x `pseq` updateState ithOut0 localState0 -- this releases "the lock" on the local state
  -- traceM $ "releasing lock -> " ++ name
  z <- y `pseq` return $ oPrint $ "releasing lock -> " ++ name0 ++ " -> " ++ show d0
  -- oput $ (GlobalState gsIn0 gsOut0) $ Set.insert i touchedState0'
  return $ z `pseq` ()

oget :: OhuaM s s
oget = OhuaM comp where
  comp s = do
    return (s,s)

oput :: s -> OhuaM s ()
oput newState = OhuaM comp where
  comp _ = do
    s <- ohuaPrint (return ()) $ "oput"
    return (s,newState)

updateState :: NFData s => IVar s -> s -> Par ()
updateState outer newState = do
  outer' <- ohuaPrint (return outer) $ "update state"
  _ <- P.put outer' newState
  return ()

getState :: IVar s -> Par s
getState outer = do
  val <- P.get outer -- will wait for the value
  return val

runOhuaM :: (NFData a, NFData s) => OhuaM (GlobalState s) a -> [s] -> (a,[s])
runOhuaM comp initialState = runPar $ do
  inState <- mapM newFull initialState
  outState <- forM initialState $ \_ -> new
  (result, _) <- runOhua comp $ GlobalState inState outState Set.empty
  finalState <- mapM getState outState
  return (result, finalState)

-- this spawns the computations for the elements but integrates the
-- state dependencies!
{-# NOINLINE smap #-}
smap :: (NFData b, NFData s, Show a) => (Int -> a -> OhuaM (GlobalState s) b) -> [a] -> OhuaM (GlobalState s) [b]
smap algo xs = do
      (GlobalState gsIn gsOut touched) <- oget -- get me the initial state
      futures <- liftPar $ smap' algo xs [0..] gsIn touched
      results <- forM futures $ \fut -> liftPar $ do -- collect the results
                                                    r <- P.get fut
                                                    return r
      let result = map fst results
      let (GlobalState _ gsOut' touchedSMap) = (last . map snd) results
      -- merge: find the local states touched by the computation f and
      --        move them over to gsOut
      gsOut'' <- liftPar $ merge gsOut gsOut' touchedSMap
      oput $ GlobalState gsIn gsOut'' $ Set.union touched touchedSMap
      return result
        where
          smap' :: (NFData b, NFData s, Show a) =>
                              (Int -> a -> OhuaM (GlobalState s) b) ->
                              [a] ->
                              [Int] ->
                              [IVar s] ->
                              Set Int ->
                              Par [IVar (b, (GlobalState s))]
          smap' f (y:ys) (ident:idents) prevState touched = do
            outS <- forM prevState $ \_ -> new -- create the new output state
            result <- spawn $ runOhua (f ident y) $ GlobalState prevState outS Set.empty
            -- traceM $ "spawned smap computation: " ++ show ident
            rest <- smap' f ys idents outS touched
            return $ [result] ++ rest
          smap' _ [] _ _ _ = return []
          merge :: (NFData s) => [IVar s] -> [IVar s] -> Set Int -> Par [IVar s]
          merge initialOut computedOut touchedSMap = do
            updateTouched computedOut initialOut touchedSMap
            return initialOut
          updateTouched :: (NFData s) => [IVar s] -> [IVar s] -> Set Int -> Par ()
          updateTouched from to touched = do
            _ <- ($) forM (Set.toAscList touched) $ \i -> do
                                      let computedLocalState = from !! i
                                      let emptyLocalState = to !! i
                                      result <- P.get computedLocalState -- is already available!
                                      P.put emptyLocalState result
                                      return ()
            return ()



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
