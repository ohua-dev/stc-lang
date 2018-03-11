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

-- TODO I still have the feeling that this monad is not yet written correctly.
--      It feels to me like there should be two monads:
--      1. the Stream monad that turns arrays into streams and
--      2. the OhuaM monad which uses the streams monad just for smap.
--      When smap :: (a -> OhuaM s b) -> [a] -> OhuaM s [b] then the challenge
--      for smap is to turn the computation given as the first parameter into a
--      streaming computation. In order to do so, we have to exchange the bind
--      operation to work on the streaming one, i.e., we need to somehow lift this
--      computation into the Stream monad.

type SFM s b = State s b

type SF s a b = a -> SFM s b

newtype OhuaM s a = OhuaM { runOhua :: s -> Par ( DFResult a -- the result queue
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
data StateResult s = Async (IVar s) | Sync s | Redundant s deriving (Generic)
instance Show s => Show (StateResult s)

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

-- FIXME see comment above: this looks like a return for lists!
streamit :: NFData a => [a] -> OhuaM s a
streamit xs = OhuaM comp
  where
    comp gs = do
      hd <- new
      tl <- foldM convert hd $ init xs
      P.put tl $ Last $ last xs
      return (DFStream hd,[Redundant gs])
    convert nextPtr x = do
      nextPtr' <- new
      P.put nextPtr $ Cons x nextPtr'
      return nextPtr'

collect :: Stream a -> Par [a]
collect chan = collect' chan []
  where
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

instance (NFData s,Show s) => Functor (OhuaM s) where
 fmap = Control.Monad.liftM

instance (NFData s,Show s) => Applicative (OhuaM s) where
 pure = return
 (<*>) = Control.Monad.ap -- TODO implement true task-level parallelism here

instance (NFData s,Show s) => Monad (OhuaM s) where
 return :: forall a.a -> OhuaM s a
 -- FIXME just does not work because what we actually need to do is turn the
 --       array into a stream. so even if this function is passed a stream then
 --       the DFStream also wants to have something that returns a list of type a
 --       instead of just an a.
 -- return l@(x:xs) = OhuaM comp
 --     where
 --       comp :: s -> Par (DFResult a,[StateResult s])
 --       comp gs = do
 --         flow <- streamit l
 --         return (DFStream flow,[Redundant gs])
 return v = OhuaM comp
     where
       comp :: s -> Par (DFResult a,[StateResult s])
       comp gs = return (Singleton v,[Redundant gs])

 (>>=) :: forall a b.OhuaM s a -> (a -> OhuaM s b) -> OhuaM s b
 f >>= g = OhuaM comp
     where
      comp :: s -> Par (DFResult b,[StateResult s]) -- this is always [IVar (s, Int)]
      comp gs = do
       (outF,stateF) <- runOhua f gs
       -- traceM $ "stateF: " ++ (show (length stateF))
       -- we always have to get the first item the normal way
       case outF of
         (DFStream stream) -> do -- here, we expect more to arrive. let's check ...
            y <- P.get stream
            case y of
              -- FIXME I'm not sure this case can ever happen. verify!
             (Last x) -> do -- nothing to be spawned! just run once and forward
               (outG,stateG) <- runOhua (g x) gs
               traceM $ "DFStream LAST " ++ (show (length stateG))
               return (outG,filterRedundantState $ stateF ++ stateG)
             (Cons x xs) -> do -- we found a stream, so spawn!
               (outG,stateG) <- execDFKernel g x xs gs
               traceM $ "DFStream CONS " ++ (show (length [stateG]))
               return (outG,filterRedundantState $ stateF ++ [stateG])
         (Singleton x) -> do -- nothing to be spawned! just run once and forward
           traceM $ "here!!! before: " ++ (show (length stateF))
           (outG,stateG) <- runOhua (g x) gs
           -- traceM $ "stateG: " ++ (show (length stateG))
           return (outG,filterRedundantState $ stateF ++ stateG)
      -- we filter here in order keep the list of updates minimal. the alternative
      -- would be to return an empty state in 'return' and 'oget'.
      filterRedundantState :: [StateResult s] -> [StateResult s]
      filterRedundantState = filter (\s -> case s of
                                    Redundant _ -> False
                                    _otherwise -> True)

execDFKernel :: forall a b s.Show s => (a -> OhuaM s b) -> a -> Stream a -> s -> Par (DFResult b,StateResult s)
execDFKernel g x xs gs = do
  ((Singleton r),_) <- runOhua (g x) gs
  tl <- trace "executing DF Kernel ..." new
  outG <- r `pseq` newFull_ $ Cons r tl
  -- outG <- newFull $ Cons x tl <<- not working because it needs the NFData restriction on an output type ("b")!
  finalStateG <- spawn_ $ execDFKernel' xs tl gs
  return (DFStream outG,Async finalStateG)
  where
    execDFKernel' :: Stream a -> Stream b -> s -> Par s
    execDFKernel' input gOutG gs = do
      i <- trace "exec DF kernel'" $ P.get input
      case i of
        (Cons x xs) -> do
          ((Singleton r),l@[(Sync gs')]) <- runOhua (g x) gs
          tl <- trace ("execKernel: " ++ (show (length l))) new
          r `pseq` P.put_ gOutG $ Cons r tl
          -- P.put gOutG $ Cons x' tl <<- not working because it needs the NFData restriction on an output type ("b")!
          execDFKernel' xs tl gs'
        (Last x) -> do
          -- ((Singleton r),[(Sync gs')]) <- runOhua (f x) gs
          out <- trace "executing sf" runOhua (g x) gs
          let (r,gs') = case out of
                ((Singleton result),[(Sync gState)]) -> (result,gState)
                ((Singleton result),l@((Sync g1):[(Sync g2)])) -> --(result,l)
                  error $ "state is a list of size: " ++ show g1 ++ show g2-- ++ show stateStuff
                -- _otherwise -> error "some weird result"
          r `pseq` P.put_ gOutG $ Last r
          -- P.put gOutG $ Last x' <<- not working because it needs the NFData restriction on an output type ("b")!
          return gs'


oget :: Show s => OhuaM s s
oget = OhuaM comp where
  comp :: Show s => s -> Par (DFResult s,[StateResult s])
  comp gs = do
    traceM $ "oget" ++ show gs
    return (Singleton gs,[Redundant gs])

oput :: Show s => s -> OhuaM s ()
oput newState = OhuaM comp where
  comp _ = do
    traceM $ "oput" ++ show newState
    return (Singleton (),[Sync newState])
  -- comp _ = return (Singleton (),[Sync (newState,-1)])

-- liftPar :: forall a s.Par a -> OhuaM (GlobalState s) a
-- liftPar p = OhuaM comp
--   where
--     comp :: GlobalState s -> Par (DFResult a, [StateResult (GlobalState s)])
--     comp gs = do
--       result <- p
--       return (Singleton result,[Sync gs])

liftWithIndex :: forall s a b.(NFData s, Show a, Show s, Show b) => String -> Int -> SF s a b -> Int -> a -> OhuaM (GlobalState s) b
liftWithIndex name fnIdx sf ident input = do
-- liftWithIndex :: (NFData s, Show a, NFData b) => String -> Int -> SF s a b -> a -> OhuaM (GlobalState s) b
-- liftWithIndex name fnIdx sf input = do
  traceM $ "running: " ++ show name ++ " data: " ++ show input ++ " ident: " ++ show ident
  (GlobalState (gs,_)) <- oget
  let (before,ls:after) = splitAt fnIdx gs
  (result,ls') <- return $ runState (sf input) ls
  oput $ GlobalState (before ++ [ls'] ++ after,fnIdx)
  traceM $ "running: " ++ show name ++ " data: " ++ show input ++ " ident: " ++ show ident ++ " DONE:" ++ show result
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
                                        Redundant resultState -> return resultState
  traceM $ show localStates
  finalState <- ((foldM f []) . (sortBy (compare `on` (\(GlobalState gs) -> snd gs)))) localStates
  return (result, finalState)
  where
    f :: [s] -> GlobalState s -> Par [s]
    f prev gs= return $ prev ++ extractState gs
    extractState (GlobalState (_,-1)) = []
    extractState (GlobalState (localGS,idx)) = let (_,s:_) = splitAt idx localGS in [s]


-- TODO how does nested smap work???
smap :: (NFData a, NFData s, Show a, Show s) => (Int -> a -> OhuaM (GlobalState s) b) -> [a] -> OhuaM (GlobalState s) [b]
smap algo xs = OhuaM comp
  where
    comp gs = do
      (DFStream outS,states) <- runOhua ((algo 1) =<< (streamit xs)) gs
      result <- trace ("collecting results ..." ++ (show (length states))) (collect outS)
      traceM $ "num results: " ++ (show (length result))
      return (Singleton result, states)
