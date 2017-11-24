-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}

--- this implementation does not rely on channels. it builds on futures!

module FuturesBasedMonad where

import Control.Monad
import Control.Monad.State as S
import Control.Monad.Par as P
import Control.DeepSeq

type SF s a = a -> State [s] a

newtype OhuaM s a = OhuaM { runOhua :: s -> (Par ( a -- the result
                                                 , s -- the global state
                                    ))
                                  }

newtype LocalStateBox s = LocalStateBox (IVar (Bool, IVar s))

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
instance Functor (OhuaM s) where
  fmap = Control.Monad.liftM

instance Applicative (OhuaM s) where
  pure = return
  (<*>) = Control.Monad.ap

instance Monad (OhuaM s) where
  return :: forall a.a -> OhuaM s a
  return v = OhuaM comp
      where
        comp :: s -> Par (a, s)
        comp gs = do
          return (v,gs)

  (>>=) :: forall a b. OhuaM s a -> (a -> OhuaM s b) -> OhuaM s b
  OhuaM comp0 >>= f = OhuaM comp
      where
        comp :: s -> Par (b, s)
        comp gs = do
          (result0, gs') <- comp0 gs -- FIXME why is this not runOhua comp0 gs ????
          let comp1 = runOhua (f result0) gs'
          ivar <- spawn_ comp1
          (result1, gs'') <- P.get ivar -- data dependency
          return (result1, gs'')

liftPar :: Par a -> OhuaM ([LocalStateBox s],[LocalStateBox s]) a
liftPar p = OhuaM comp
  where
    comp s = do
      result <- p
      return (result,s)

liftWithIndex :: NFData s => Int -> (a -> State s b) -> a -> OhuaM ([LocalStateBox s],[LocalStateBox s]) b
liftWithIndex i f d = do
  -- we define the proper order on the private state right here!
  (gsIn,gsOut) <- oget
  let (_,ithIn:_) = splitAt i gsIn
  let (_,ithOut:_) = splitAt i gsOut
  localState <- liftPar $ getState ithIn -- this synchronizes access to the local state
  liftPar $ touchState ithOut -- identify the state used
  let (d', localState') = runState (f d) localState
  liftPar $ updateState ithOut localState' -- this releases "the lock" on the local state
  return d'


oget :: OhuaM s s
oget = OhuaM comp where
  comp s = do
    return (s,s)

oput :: s -> OhuaM s ()
oput newState = OhuaM comp where
  comp oldState = do
    return ((),newState)


initLocalState :: s -> Par (LocalStateBox s)
initLocalState state = do
  outer <- new
  inner <- new
  -- (outer,inner) <- sequence [new,new]
  P.put outer (False, inner)
  return $ LocalStateBox outer

updateState :: NFData s => LocalStateBox s -> s -> Par ()
updateState (LocalStateBox outer) newState = do
    (_,inner) <- P.get outer -- never has to wait
    P.put inner newState
    return ()

getState :: LocalStateBox s -> Par s
getState (LocalStateBox outer) = do
    (_,inner) <- P.get outer -- never has to wait
    val <- P.get inner -- will wait for the value
    return val

touchState :: LocalStateBox s -> Par ()
touchState (LocalStateBox outer) = do
    (touched,inner) <- P.get outer -- never has to wait
    case touched of -- TODO this might just be an assertion
      True -> do P.put outer (True,inner)
      False -> return () -- no multiple writes allowed

wasStateTouched :: LocalStateBox s -> Par Bool
wasStateTouched (LocalStateBox outer) = do
  (touched,_) <- P.get outer -- never has to wait
  return touched

runOhuaM :: OhuaM ([LocalStateBox s],[LocalStateBox s]) a -> [s] -> (a,[s])
runOhuaM comp state = runPar $ do
  inState <- mapM initLocalState state
  outState <- mapM initLocalState state
  (result, _) <- runOhua comp (inState,outState)
  finalState <- mapM getState outState
  return (result, finalState)

-- this spawns the computations for the elements but integrates the
-- state dependencies!
smap :: (a -> OhuaM ([LocalStateBox s],[LocalStateBox s]) b) -> [a] -> OhuaM ([LocalStateBox s],[LocalStateBox s]) [b]
smap f xs = do
      (gsIn,gsOut) <- oget -- get me the initial state
      result <- smap' f xs gsIn
      (_,gsOut') <- oget -- get me the computed state
      -- merge: find the local states touched by the computation f in newGsState and
      --        move them over to gsOut
      gsOut'' <- liftPar $ merge gsOut gsOut'
      oput (gsIn,gsOut'')

      return result
        where
          smap' :: (a -> OhuaM ([LocalStateBox s],[LocalStateBox s]) b) -> [a] -> [LocalStateBox s] -> OhuaM ([LocalStateBox s],[LocalStateBox s]) [b]
          smap' f (x:xs) prevState = do
            outS <- forM prevState (\s -> liftPar $ do  -- create the new output state
                                                    state <- getState s
                                                    initLocalState state) 
            -- FIXME this needs to run in parallel and therefore needs to be spawned!
            oput (prevState, outS) -- FIXME not sure whether this will work because all recursive calls will set this state.
            result <- f x
            rest <- smap' f xs outS
            return $ [result] ++ rest
            -- return $ [f x (prevState,outS)] ++ smap' f xs outS
          smap' f [] prevState = do
            -- this is the final state. report it back to the smap function.
            (gsIn, _) <- oget
            oput (gsIn, prevState)
            return []
          merge :: [LocalStateBox s] -> [LocalStateBox s] -> Par [LocalStateBox s]
          merge initialOut computedOut = mapM swapUsed $ zip initialOut computedOut
            where
              swapUsed :: (LocalStateBox s,LocalStateBox s) -> Par (LocalStateBox s)
              swapUsed (i,c) = do
                used <- wasStateTouched c
                case used of
                  True -> return c
                  False -> return i

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
