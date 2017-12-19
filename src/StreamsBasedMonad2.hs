{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}

--- this implementation relies on channels. it more closely mimics a static dataflow graph.

module StreamsBasedMonad2 where

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

data IList a = Cons a (IVar (IList a)) | Last a | End deriving (Generic)
newtype SList a = SList (IVar (IList a))
instance NFData a => NFData (IList a)

class Stream m where
  pull :: m a -> Par (a,m a)
  push :: m a -> a -> Par (m a)

instance Stream [] where
  pull :: [a] -> Par (a,[a])
  pull [] = return (undefined,[])
  pull (x:xs) = return (x,xs)

  push :: [a] -> a -> Par [a]
  push xs x = return $ xs ++ [x]

instance Stream SList where
  pull :: SList a -> Par (a, SList a)
  pull (SList inS) = do
    v <- P.get inS
    case v of
      Cons x xs -> return (x, SList xs)
      Last x -> return (x, SList End)

  push :: SList a -> a -> Par (SList a)
  push outS x = do
    nextPtr <- new
    P.put outS $ Cons x nextPtr
    return nextPtr


stream :: (NFData a) => [a] -> Par (SList a)
stream xs = do
  hd <- new
  tl <- foldM convert hd $ init xs
  P.put tl $ Last $ last xs
  return hd
  where
    convert nextPtr x = do
      nextPtr' <- new
      P.put nextPtr $ Cons x nextPtr'
      return nextPtr'

collect :: SList a -> Par [a]
collect chan = collect' chan []
  where
    collect' :: IVar (IList a) -> [a] -> Par [a]
    collect' c r = do
      i <- P.get c
      case i of
        Last x -> return $ r ++ [x]
        Cons x xs -> collect' xs $ r ++ [x]


newtype OhuaM s a = OhuaM { runOhua :: forall m.(Stream m) => s -> Par ( m a -- the result queue
                                                , [IVar s] -- the global state
                                                )
                          }

newtype GlobalState s = GlobalState ( [s] -- the global state array
                                    , Int) -- the index of the entry that was changed
                                    deriving (Generic,Show)
instance NFData s => NFData (GlobalState s)

instance (NFData s,Show s) => Functor (OhuaM s) where
 fmap = Control.Monad.liftM

instance (NFData s,Show s) => Applicative (OhuaM s) where
 pure = return
 (<*>) = Control.Monad.ap -- TODO implement true task-level parallelism here

instance (NFData s,Show s) => Monad (OhuaM s) where
 return :: forall a m.a -> OhuaM s a
 return v = OhuaM comp
     where
       comp :: Stream t => s -> Par (t a,[IVar s])
       comp gs = do
         st <- newFull gs
         outS <- stream [v]
         return (outS,[st])

 (>>=) :: forall a b m.Stream m => OhuaM s a -> (m a -> OhuaM s b) -> OhuaM s b
 f >>= g = OhuaM comp
     where
      comp ::(Stream m) => s -> Par (m b,[IVar s]) -- this is always [IVar (s, Int)]
      comp gs = do
       (outF,stateF) <- runOhua f gs
       (outG,stateG) <- runOhua (g outF) gs
       return (outG,stateF ++ stateG)

oget :: Show s => OhuaM s s
oget = OhuaM comp where
  comp :: (Show s) => s -> Par (IVar (IList s),[IVar s])
  comp gs = do
    traceM $ "oget" ++ show gs
    return (stream [gs],[newFull gs])

oput :: Show s => s -> OhuaM s ()
oput newState = OhuaM comp where
  comp _ = do
    traceM $ "oput" ++ show newState
    return (stream [],[newFull newState])

execDFKernel :: (Show s, Stream m) => (a -> OhuaM (GlobalState s) b) -> m a -> s -> Par (m b,IVar s)
execDFKernel sf inS gs = do
  outS <- stream []
  finalState <- spawn_ $ execDFKernel' inS outS gs
  return (outS,finalState)
  where
    execDFKernel' :: Stream a -> Stream b -> s -> Par s
    execDFKernel' inS0 outS0 gs0 = do
      i <- trace "exec DF kernel'" $ P.get inS0
      case i of
        (Cons x xs) -> do
          (r,gs0') <- runOhua (sf x) gs0
          tl <- trace ("execKernel: " ++ (show (length l))) new
          r `pseq` P.put_ outS0 $ Cons r tl
          execDFKernel' xs tl gs0'
        (Last x) -> do
          (r,gs0') <- trace "executing sf" runOhua (sf x) gs0
          r `pseq` P.put_ outS0 $ Last r
          return gs0'

execSF :: (NFData s, Show a, Show s, Show b) => String -> Int -> SF s a b -> Int -> a -> OhuaM (GlobalState s) b
execSF name fnIdx sf ident input = do
  traceM $ "running: " ++ show name ++ " data: " ++ show input ++ " ident: " ++ show ident
  (GlobalState (gs,_)) <- oget
  let (before,ls:after) = splitAt fnIdx gs
  (result,ls') <- return $ runState (sf input) ls
  oput $ GlobalState (before ++ [ls'] ++ after,fnIdx)
  traceM $ "running: " ++ show name ++ " data: " ++ show input ++ " ident: " ++ show ident ++ " DONE:" ++ show result
  return result

liftWithIndex :: (NFData s, Show a, Show s, Show b, Stream m) => String -> Int -> SF s a b -> Int -> m a -> OhuaM (GlobalState s) b
liftWithIndex name fnIdx sf ident input = OhuaM $ \gs -> execDFKernel (execSF name fnIdx sf ident) input gs

runOhuaM :: (NFData a, NFData s, Show s) => OhuaM (GlobalState s) a -> [s] -> (a,[s])
runOhuaM comp initialState = runPar $ do
  (resultS,states) <- runOhua comp $ GlobalState (initialState,-1)
  [result] <- collect resultS
  localStates <- mapM P.get states
  -- traceM $ show localStates
  finalState <- ((foldM f []) . (sortBy (compare `on` (\(GlobalState gs) -> snd gs)))) localStates
  return (result, finalState)
  where
    f :: [s] -> GlobalState s -> Par [s]
    f prev gs= return $ prev ++ extractState gs
    extractState (GlobalState (_,-1)) = []
    extractState (GlobalState (localGS,idx)) = let (_,s:_) = splitAt idx localGS in [s]


-- TODO how does nested smap work???
-- smap :: (NFData a, NFData s, Show a, Show s) => (Int -> a -> OhuaM (GlobalState s) b) -> [a] -> OhuaM (GlobalState s) [b]
-- smap algo xs = OhuaM comp
--   where
--     comp gs = do
--       (DFStream outS,states) <- runOhua ((algo 1) =<< (streamit xs)) gs
--       result <- trace ("collecting results ..." ++ (show (length states))) (collect outS)
--       traceM $ "num results: " ++ (show (length result))
--       return (Singleton result, states)
