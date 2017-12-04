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
import Control.Parallel (pseq)
import Data.List
import Data.Function

type SFM s b = State s b

type SF s a b = a -> SFM s b

newtype OhuaM s a = OhuaM { runOhua :: s -> Par (DFResult a -- the result queue
                                                , [StateResult s] -- the global state
                                                )
                          }

newtype GlobalState s = GlobalState ([s] -- the global state array
                                    ,Int) -- the index of the entry that was changed
                                    deriving (Generic,Show)
instance NFData s => NFData (GlobalState s)

data IList a = Cons a (IVar (IList a)) | Last a deriving (Generic)
type Stream a = IVar (IList a)
instance NFData a => NFData (IList a)

data DFResult a = DFStream (Stream a) | Singleton a deriving (Generic)
data StateResult s = Async (IVar s) | Sync s deriving (Generic)

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

-- collect :: Stream a -> Par [a]
-- collect chan = collect' chan [] where
--   collect' :: Stream a -> [a] -> Par [a]
--   collect' c r = do
--     i <- P.get c
--     case i of
--       Last x -> return $ r ++ [x]
--       Cons x xs -> collect' xs $ r ++ [x]

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
       comp gs = return (Singleton v,[Sync gs])

 (>>=) :: forall a b.OhuaM s a -> (a -> OhuaM s b) -> OhuaM s b
 f >>= g = OhuaM comp
     where
      comp :: s -> Par (DFResult b,[StateResult s]) -- this is always [IVar (s, Int)]
      comp gs = do
       (outF,stateF) <- runOhua f gs
       -- we always have to get the first item the normal way
       case outF of
         (DFStream stream) -> do -- here, we expect more to arrive. let's check ...
            y <- P.get stream
            case y of
              -- FIXME I'm not sure this case can ever happen. verify!
             (Last x) -> do -- nothing to be spawned! just run once and forward
               (outG,stateG) <- runOhua (g x) gs
               return (outG,stateF ++ stateG)
             (Cons x xs) -> do -- we found a stream, so spawn!
               (outG,stateG) <- execG x xs gs
               return (outG,stateF ++ [stateG])
         (Singleton x) -> do -- nothing to be spawned! just run once and forward
           (outG,stateG) <- runOhua (g x) gs
           return (outG,stateF ++ stateG)
      execG :: a -> Stream a -> s -> Par (DFResult b,StateResult s)
      execG x xs gs = do
        ((Singleton r),_) <- runOhua (g x) gs
        tl <- new
        outG <- r `pseq` newFull_ $ Cons r tl
        -- outG <- newFull $ Cons x tl <<- not working because it needs the NFData restriction on an output type ("b")!
        finalStateG <- spawn $ execG' xs tl gs
        return (DFStream outG,Async finalStateG)
      execG' :: Stream a -> Stream b -> s -> Par s
      execG' input gOutG gs = do
        i <- P.get input
        case i of
          (Cons x xs) -> do
            ((Singleton r),[(Sync gs')]) <- runOhua (g x) gs
            tl <- new
            r `pseq` P.put_ gOutG $ Cons r tl
            -- P.put gOutG $ Cons x' tl <<- not working because it needs the NFData restriction on an output type ("b")!
            execG' xs tl gs'
          (Last x) -> do
            ((Singleton r),[(Sync gs')]) <- runOhua (g x) gs
            r `pseq` P.put_ gOutG $ Last r
            -- P.put gOutG $ Last x' <<- not working because it needs the NFData restriction on an output type ("b")!
            return gs'

oget :: OhuaM s s
oget = OhuaM comp where
  comp :: s -> Par (DFResult s,[StateResult s])
  comp gs = do
    return (Singleton gs,[Sync gs])

oput :: Show s => s -> OhuaM s ()
oput newState = OhuaM comp where
  comp _ = do
    traceM $ "oput" ++ show newState
    return (Singleton (),[Sync newState])
  -- comp _ = return (Singleton (),[Sync (newState,-1)])

liftPar :: forall a s.Par a -> OhuaM (GlobalState s) a
liftPar p = OhuaM comp
  where
    comp :: GlobalState s -> Par (DFResult a, [StateResult (GlobalState s)])
    comp gs = do
      result <- p
      return (Singleton result,[Sync gs])

liftWithIndex :: forall s a b.(NFData s, Show a, Show s) => String -> Int -> SF s a b -> Int -> a -> OhuaM (GlobalState s) b
liftWithIndex name fnIdx sf ident input = do
-- liftWithIndex :: (NFData s, Show a, NFData b) => String -> Int -> SF s a b -> a -> OhuaM (GlobalState s) b
-- liftWithIndex name fnIdx sf input = do
  (GlobalState (gs,_)) <- oget
  let (before,ls:after) = splitAt fnIdx gs
  (result,ls') <- return $ runState (sf input) ls
  oput $ GlobalState (before ++ [ls'] ++ after,fnIdx)
  return result

runOhuaM :: (NFData a, NFData s, Show s) => OhuaM (GlobalState s) a -> [s] -> (a,[s])
runOhuaM comp initialState = runPar $ do
  (outS,states) <- runOhua comp $ GlobalState (initialState,-1)
  result <- case outS of
    Singleton x -> return x
    DFStream _ -> error "impossible"
  localStates <- forM states $ \s -> case s of
                                        Sync resultState -> return resultState
                                        Async resultState -> P.get resultState
  traceM $ show localStates
  finalState <- ((foldM f []) . (sortBy (compare `on` (\(GlobalState gs) -> snd gs)))) localStates
  return (result, finalState)
  where
    f :: [s] -> GlobalState s -> Par [s]
    f prev gs= return $ prev ++ extractState gs
    extractState (GlobalState (_,-1)) = []
    extractState (GlobalState (localGS,idx)) = let (_,s:_) = splitAt idx localGS in [s]
