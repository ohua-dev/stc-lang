{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--- this implementation does not rely on channels. it builds on futures!

module SfOperations (SF, smap) where

import Control.Monad.State as S
import Control.Monad.Par as P
import Control.DeepSeq

type SF s a = a -> State [s] a

newtype OhuaM s a = OhuaM { runOhua :: s -> (Par ( IVar a -- the result
                                                 , s -- the global state
                                    ))
                                  }

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
instance Monad (OhuaM s) where
  return :: a -> OhuaM s a
  return v = OhuaM comp
      where
        comp gs = do
          return (v,s)

  (>>=) :: OhuaM s a -> (a -> OhuaM s b) -> OhuaM s b
  OhuaM comp0 >>= f = OhuaM comp
      where
        comp gs = do
          let (result0, gs') = runOhua comp0 gs
          v <- P.get result0 -- fetch the result
          let (result1, gs'') <- spawn $ do f v gs
          return (result1, gs'')

oget :: OhuaM s a
oget = OhuaM comp where
  comp s = do
    ivar <- new
    P.put ivar s
    return (ivar,s)

oput :: s -> OhuaM s a
oput newState = OhuaM comp where
  comp oldState = do
    ivar <- new
    P.put ivar ()
    return (ivar,newState)

newtype LocalStateBox s = LocalStateBox (IVar (Bool, IVar s))

initLocalState :: s -> Par LocalStateBox s
initLocalState state = do
  (outer,inner) <- sequence [new,new]
  P.put outer (False, inner)
  return LocalStateBox outer

updateState :: LocalStateBox s -> s -> Par ()
touchState outer newState = do
  (_,inner) <- P.get outer -- never has to wait
  P.put inner newState

getState :: LocalStateBox s -> Par s
touchState outer = do
  (_,inner) <- P.get outer -- never has to wait
  val <- P.get inner -- will wait for the value
  return val

touchState :: LocalStateBox s -> Par ()
touchState outer = do
  (touched,inner) <- P.get outer -- never has to wait
  case touched of -- TODO this might just be an assertion
    True -> do P.put outer (True,inner)
    False -> _ -- no multiple writes allowed
  return ()

wasStateTouched :: LocalStateBox s -> Par Bool
wasStateTouched outer = do
  (touched,_) <- P.get outer -- never has to wait
  return touched

liftWithIndex :: Int -> (a -> State s b) -> a -> OhuaM ([LocalStateBox s],[LocalStateBox s]) b
liftWithIndex i f d = do
  -- we define the proper order on the private state right here!
  (gsIn,gsOut) <- oget
  let (_,ithIn:_) = splitAt i gsIn
  let (_,ithOut:_) = splitAt ith gsOut
  localState <- getState ithIn -- this synchronizes access to the local state
  touchState ithOut -- indentify the state used
  let (d', localState') = runState (f d) localState
  updateState ithOut' localState' -- this releases "the lock" on the local state
  return d'

runOhuaM :: OhuaM ([LocalStateBox s],[LocalStateBox s]) a -> [s] -> (a,[s])
runOhuaM comp state = runPar $ do
  inState <- mapM initLocalState state
  outState <- mapM initLocalState state
  let (result, _) = comp (inState,outState)
  finalState <- mapM getState outState
  return result

-- this spawns the computations for the elements but integrates the
-- state dependencies!
smap :: (a -> OhuaM ([LocalStateBox s],[LocalStateBox s]) b) -> [a] -> OhuaM ([LocalStateBox s],[LocalStateBox s]) [b]
smap f xs = do
  (gsIn,gsOut) <- oget -- get me the initial state
  (result,gsOut') <- smap' f xs gsIn

  -- merge: find the local states touched by the computation f in newGsState and
  --        move them over to gsOut
  oput $ merge gsOut gsOut'

  return result
  where
    smap' :: (a -> OhuaM ([LocalStateBox s],[LocalStateBox s]) b) -> [a] -> [LocalStateBox s] -> OhuaM ([LocalStateBox s],[LocalStateBox s]) [b]
    smap' f (x:xs) prevState = do
      outS <- mapM createLocalState prevState -- create the new output state
      return [f x (prevState,outS)] ++ $ smap' f xs outS
    smap' f [] prevState = do
      -- this is the final state. report it back to the smap function.
      (gsIn, _) <- oget
      oput (gsIn, prewState)
      return []
    merge :: [LocalStateBox s] -> [LocalStateBox s] -> Par [LocalStateBox s]
    merge initialOut computedOut = mapM swapUsed $ zip initialOut computedOut
      where
        swapUsed :: LocalStateBox s -> LocalStateBox s -> Par LocalStateBox s
        swapUsed i c = do
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
