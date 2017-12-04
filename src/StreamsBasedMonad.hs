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
import Control.Parallel (pseq)
import Data.List
import Data.Function

type SFM s b = State s b

type SF s a b = a -> SFM s b

newtype OhuaM s a = OhuaM { runOhua :: s -> Par (DFResult a -- the result queue
                                                , [StateResult s] -- the global state
                                                )
                          }

newtype GlobalState s = GlobalState [s] deriving (Generic)
instance NFData s => NFData (GlobalState s)

data IList a = Cons a (IVar (IList a)) | Last a deriving (Generic)
type Stream a = IVar (IList a)
instance NFData a => NFData (IList a)

data DFResult a = DFStream (Stream a) | Singleton a deriving (Generic)
data StateResult s = Async (IVar (s,Int)) | Sync (s,Int) deriving (Generic)

-- emptyStream :: Par (Stream a)
-- emptyStream = new

-- readData :: Stream a -> Par (IList a)
-- readData s = P.get s

-- emitData :: NFData a => Stream a -> a -> Par (Stream a)
-- emitData output result = do
--   newPendingTail <- P.new
--   P.put output $ Cons result newPendingTail
--   return newPendingTail

-- emitNull :: NFData a => Stream a -> Par ()
-- emitNull output = P.put output Null

-- stream :: NFData a => [a] -> Par (Stream a)
-- stream [] = do
--   i <- new
--   P.put i Null
--   return i
-- stream xs = do
--   hd <- new
--   tl <- foldM convert hd xs
--   P.put tl Null
--   return hd
--   where
--     convert nextPtr x = do
--       nextPtr' <- new
--       P.put nextPtr $ Cons x nextPtr'
--       return nextPtr'

collect :: Stream a -> Par [a]
collect chan = collect' chan [] where
  collect' :: Stream a -> [a] -> Par [a]
  collect' c r = do
    i <- P.get c
    case i of
      Last x -> return $ r ++ [x]
      Cons x xs -> collect' xs $ r ++ [x]

-- runDFKernel :: (NFData a, NFData b) => Stream a -> Stream b -> SF s a b -> s -> Par s
-- runDFKernel input output f state = do
--   i <- readData input -- retrieve an element from the stream
--   case i of
--     Null -> do -- propagate the EOS and shut down
--       emitNull output
--       return state
--     (Cons e es) -> do -- run the computation and emit the result
--       let (result,state') = runState (f e) state
--       newOutput <- emitData output result
--       runDFKernel es newOutput f state'

-- runSF :: SF s a b -> s -> a -> Par (b,s)
-- runSF f state input = runState (f input) state

instance (NFData s) => Functor (OhuaM s) where
 fmap = Control.Monad.liftM

instance NFData s => Applicative (OhuaM s) where
 pure = return
 (<*>) = Control.Monad.ap -- TODO implement true task-level parallelism here

instance NFData s => Monad (OhuaM s) where
 return :: forall a.a -> OhuaM s a
 return v = OhuaM comp
     where
       comp :: s -> Par (DFResult a,[StateResult s])
       comp gs = return (Singleton v,[])

 (>>=) :: forall a b.OhuaM s a -> (a -> OhuaM s b) -> OhuaM s b
 f >>= g = OhuaM comp
     where
      comp :: s -> Par (DFResult b,[StateResult s]) -- this is always [IVar (s, Int)]
      comp gs = do
       (outF,stateF) <- runOhua f gs
       -- we always have to get the first item the normal way
       case outF of
         (DFStream x) -> do -- here, we expect more to arrive. let's check ...
            y <- P.get x
            case y of
              -- FIXME I'm not sure this case can ever happen. verify!
             (Last x) -> do -- nothing to be spawned! just run once and forward
               (outG,stateG) <- runOhua (g x) gs
               return (outG,stateF ++ stateG)
             (Cons x xs) -> do -- we found a stream, so spawn!
               (outG,stateG) <- execG g x xs gs
               return (outG,stateF ++ [stateG])
         (Singleton x) -> do -- nothing to be spawned! just run once and forward
           (outG,stateG) <- runOhua (g x) gs
           return (outG,stateF ++ stateG)
      execG :: (a -> OhuaM s b) -> a -> Stream a -> s -> Par (DFResult b,StateResult s)
      execG g x xs gs = do
        ((Singleton x),stateG) <- runOhua (g x) gs
        tl <- new
        outG <- x `pseq` newFull_ $ Cons x tl
        -- outG <- newFull $ Cons x tl <<- not working because it needs the NFData restriction on an output type ("b")!
        finalStateG <- spawn $ execG' xs tl gs
        return (DFStream outG,Async finalStateG)
      execG' :: Stream a -> Stream b -> s -> Par (s, Int)
      execG' input gOutG gs = do
        i <- P.get input
        case i of
          (Cons x xs) -> do
            ((Singleton x'),[(Sync (gs',_))]) <- runOhua (g x) gs
            tl <- new
            x' `pseq` P.put_ gOutG $ Cons x' tl
            -- P.put gOutG $ Cons x' tl <<- not working because it needs the NFData restriction on an output type ("b")!
            execG' xs tl gs'
          (Last x) -> do
            ((Singleton x'),[(Sync gs')]) <- runOhua (g x) gs
            x' `pseq` P.put_ gOutG $ Last x'
            -- P.put gOutG $ Last x' <<- not working because it needs the NFData restriction on an output type ("b")!
            return gs'

oget :: OhuaM s s
oget = OhuaM comp where
  comp :: s -> Par (DFResult s,[StateResult s])
  comp s = do
    return (Singleton s,[Sync (s,-1)])

oput :: s -> OhuaM s ()
oput newState = OhuaM comp where
  -- comp _ = do
  --   s <- ohuaPrint (return ()) $ "oput"
  --   return (s,newState)
  comp _ = return (Singleton (),[Sync (newState,-1)])

liftPar :: forall a s.Par a -> OhuaM (GlobalState s) a
liftPar p = OhuaM comp
  where
    comp :: GlobalState s -> Par (DFResult a, [StateResult (GlobalState s)])
    comp state = do
      result <- p
      return (Singleton result,[Sync (state,-1)])


liftWithIndex :: (NFData s, Show a, NFData b) => String -> Int -> SF s a b -> a -> OhuaM (GlobalState s) b
liftWithIndex name fnIdx sf input = do
  (GlobalState gs) <- oget
  let (before,state:after) = splitAt fnIdx gs
  (result,state') <- return $ runState (sf input) state
  oput $ GlobalState $ before ++ [state'] ++ after
  return result

runOhuaM :: (NFData a, NFData s, NFData b) => OhuaM (GlobalState s) a -> [s] -> (b,[s])
runOhuaM comp initialState = runPar $ do
  (outS,states) <- runOhua comp $ GlobalState initialState
  result <- case outS of
    Singleton x -> return [x]
    DFStream x -> collect x
  localStates <- forM states $ \state -> case state of
                                            Sync resultState -> return resultState
                                            Async resultState -> P.get resultState
  finalState <- ((foldM f []) . (mapM fst) . (sortBy (compare `on` snd))) localStates
  return (result, finalState)
  where
    f :: [s] -> [GlobalState [s]] -> [s]
    f prev (localGS,idx)= let (_,(GlobalState state):_) = splitAt idx localGS in prev ++ [state]
