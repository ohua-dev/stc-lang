{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

--- this implementation relies on channels. it more closely mimics a static dataflow graph.

module StreamsBasedMonad where

import Control.Monad
import Control.Monad.State as S
import Control.Monad.Par as P
--
-- for debugging only:
-- import Scheduler as P
-- import Control.DeepSeq
--
import GHC.Generics (Generic)
import Debug.Trace
import Data.Set as Set hiding (map)

type SFM s b = State s b

type SF s a b = a -> SFM s b

type OhuaM s a = OhuaM { runOhua :: s -> Par ((IVar s) -- the global state
                                              , Stream a -- the result queue
                                              )
                       }

type GlobalState s = undefined -- TODO

data IList a = Null | Cons a (IVar (IList a)) | Head (IVar (IList a))
type Stream a = IVar (IList a)

emptyStream :: Par (Stream a)
emptyStream = do
  ivar <- new
  return $ Head ivar

stream :: [a] -> Par (Stream a)
stream [] = do
  i <- new
  P.put i Null
  return i
stream (x:xs) = do
  nextPtr0 <- new
  let start = Cons x nextPtr0
  foldM convert nextPtr0 xs where
  convert nextPtr0' x' = do
    nextPtr1 <- new
    P.put nextPtr0' Cons x' nextPtr1
    return nextPtr1

collect :: Stream a -> Par [a]
collect chan = collect' chan [] where
  collect' c r = do
    i <- lift $ P.get c
    case i of
      Null -> return r
      Cons e es -> collect' es $ r ++ [e]

readData :: Stream a -> Par a
readData = P.get

emitData :: Stream a -> a -> Par (IVar a)
emitData output result = do
  newPendingTail <- P.new
  P.put output $ Cons result newPendingTail
  return newPendingTail

emitNull :: Stream a -> Par ()
emitNull output = P.put output Null

runDFKernel :: (NFData a, NFData b) => Stream a -> Stream b -> SF s a b -> s -> Par s
runDFKernel input output f state = do
  i <- readData input -- retrieve an element from the stream
  case i of
    Null -> do -- propagate the EOS and shut down
      emitNull output
      return state
    Cons e es -> do -- run the computation and emit the result
      let (result,state') = runState (f e) state
      newOutput <- emitData output result
      dff es newOutput f state'

instance (NFData s) => Functor (OhuaM s) where
 fmap = Control.Monad.liftM

instance NFData s => Applicative (OhuaM s) where
 pure = return
 (<*>) = Control.Monad.ap -- TODO implement true task-level parallelism here

instance NFData s => Monad (OhuaM s) where

 return :: forall a.a -> OhuaM s a
 return v = OhuaM comp
     where
       comp :: s -> Par ([(IVar s, Int)], Stream a)
       comp gs = return ([], stream [v])

 (>>=) :: forall a b.OhuaM s a -> (Par (Stream a) -> OhuaM s b) -> OhuaM s b
 f >>= g = OhuaM comp
     where
       comp :: s -> Par ([(IVar s, Int)], Stream a)
       comp gs = do
         -- wire the two computations here
         (stateUpdateF, outF) <- runOhua f gs
         (stateUpdateG, outG) <- runOhua (g outF) gs
         return (stateUpdateG ++ stateUpdateF , outG)


liftWithIndex :: String -> Int -> SF s a b -> Stream a -> OhuaM [s] b
liftWithIndex name stateIdx sf input = OhuaM comp
  where
    comp :: s -> Par ([(IVar s, Int)], Stream a)
    comp gs = do
      output <- emptyStream
      let (_,ithIn:_) = splitAt i gs
      state <- spawn $ runDFKernel input output sf ithIn
      return ([(state,stateIdx)], output)

runOhuaM :: (NFData a, NFData s) => OhuaM (GlobalState s) a -> [s] -> (a,[s])
runOhuaM comp initialState = runPar $ do
  (states,outS) <- runOhua comp initialState
  result <- collect outS
  localStates <- forM states $ \(s,i) -> do
                                      state <- P.get s
                                      return (state,i)
  finalState <- mapM fst $ sortBy (compare `on` snd) localStates
  return (result, finalState)
