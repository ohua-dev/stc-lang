{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

--- this implementation does not rely on channels. it builds on futures!

module FuturesBasedMonad where

import Control.Monad
import Control.Monad.State as S
import Control.Monad.Par as P
import Control.DeepSeq
import GHC.Generics (Generic)
import Debug.Trace
import Data.Set as Set hiding (map)

type SF s a b = a -> State s b

newtype OhuaM s a = OhuaM { runOhua :: s -> (Par ( a -- the result
                                                 , s -- the global state
                                                 ))
                                               }
data GlobalState s = GlobalState [IVar s] [IVar s] (Set.Set Int) deriving (Generic)
instance NFData a => NFData (GlobalState a)

-- newtype LocalStateBox s = LocalStateBox (IVar (Bool, IVar s)) deriving (Generic)
--
-- instance NFData a => NFData (LocalStateBox a)

-- instance NFData a => NFData (Bucket a) where
--   rnf (Bucket a as) = a `deepseq` rnf as

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
  return :: forall a.a -> OhuaM s a
  return v = OhuaM comp
      where
        comp :: s -> Par (a, s)
        comp gs = do
          traceM $ "running return"
          return (v,gs)

  (>>=) :: forall a b.OhuaM s a -> (a -> OhuaM s b) -> OhuaM s b
  f >>= g = OhuaM comp
      where
        comp :: s -> Par (b, s)
        comp gs = do
          traceM $ "running bind"
          -- there is no need to spawn here!
          -- pipeline parallelism is solely created by smap.
          -- task-level parallelism is solely created by <*>
          (result0, gs') <- runOhua f gs
          traceM $ "ran first computation"
          (result1, gs'') <- runOhua (g result0) gs'
          traceM $ "ran second computation"
          -- ivar0 <- spawn $ runOhua f gs
          -- (result0, gs') <- P.get ivar0 -- data dependency
          -- traceM $ "ran first computation"
          -- let comp1 = runOhua (g result0) gs'
          -- ivar1 <- spawn comp1
          -- (result1, gs'') <- P.get ivar1 -- data dependency
          return (result1, gs'')

liftPar :: Par a -> OhuaM (GlobalState s) a
liftPar p = OhuaM comp
  where
    comp s = do
      traceM "called liftpar"
      result <- p
      return (result,s)

liftWithIndex :: NFData s => Int -> (a -> State s b) -> a -> OhuaM (GlobalState s) b
liftWithIndex i f d = do
  traceM $ "computation started"
  -- we define the proper order on the private state right here!
  (GlobalState gsIn gsOut touchedState) <- oget
  let (_,ithIn:_) = splitAt i gsIn
  let (_,ithOut:_) = splitAt i gsOut
  localState <- liftPar $ getState ithIn -- this synchronizes access to the local state
  let (d', localState') = runState (f d) localState
  liftPar $ updateState ithOut localState' -- this releases "the lock" on the local state
  oput $ (GlobalState gsIn gsOut) $ Set.insert i touchedState
  traceM $ "computation done"
  return d'


oget :: OhuaM s s
oget = OhuaM comp where
  comp s = do
    traceM "called oget"
    return (s,s)

oput :: s -> OhuaM s ()
oput newState = OhuaM comp where
  comp oldState = do
    traceM "called oput"
    return ((),newState)

updateState :: NFData s => IVar s -> s -> Par ()
updateState outer newState = do
  traceM "called updateState"
  P.put outer newState
  return ()

getState :: IVar s -> Par s
getState outer = do
  traceM "called getState"
  val <- P.get outer -- will wait for the value
  traceM "returning from getState"
  return val

runOhuaM :: (NFData a, NFData s) => OhuaM (GlobalState s) a -> [s] -> (a,[s])
runOhuaM comp state = runPar $ do
  inState <- mapM newFull state
  outState <- forM state $ \s -> new
  (result, _) <- runOhua comp $ GlobalState inState outState Set.empty
  finalState <- mapM getState outState
  return (result, finalState)

-- this spawns the computations for the elements but integrates the
-- state dependencies!
smap :: (NFData b, NFData s) => (a -> OhuaM (GlobalState s) b) -> [a] -> OhuaM (GlobalState s) [b]
smap f xs = do
      (GlobalState gsIn gsOut touched) <- oget -- get me the initial state
      futures <- smap' f xs gsIn
      results <- forM futures $ \fut -> liftPar $ do -- collect the results
                                                    res <- fut
                                                    r <- P.get res
                                                    return r
      let result = map fst results
      let (GlobalState _ gsOut' touchedSMap) = (last . map snd) results
      -- merge: find the local states touched by the computation f and
      --        move them over to gsOut
      gsOut'' <- liftPar $ merge gsOut gsOut' touchedSMap
      oput $ GlobalState gsIn gsOut'' $ Set.union touched touchedSMap
      return result
        where
          smap' :: (NFData b, NFData s) =>
                              (a -> OhuaM (GlobalState s) b) ->
                              [a] ->
                              [IVar s] ->
                              OhuaM (GlobalState s)
                                    [Par (IVar (b, (GlobalState s)))]
          smap' f (x:xs) prevState = do
            outS <- forM prevState (\s -> liftPar $ do  -- create the new output state
                                                    state <- getState s
                                                    newFull state)
            let result = spawn $ runOhua (f x) $ GlobalState prevState outS Set.empty
            rest <- smap' f xs outS
            return $ [result] ++ rest
          smap' _ [] _ = return []
          merge :: (NFData s) => [IVar s] -> [IVar s] -> Set Int -> Par [IVar s]
          merge initialOut computedOut touchedSMap = do
            forM (Set.toAscList touchedSMap) $ \i -> do
                                      let computed = computedOut !! i
                                      let empty = initialOut !! i
                                      result <- P.get computed -- is already available!
                                      P.put empty result
                                      return ()
            return initialOut

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
