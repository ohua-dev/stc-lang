{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SfOperations (SF, smap) where

import Control.Monad.State as S
import Control.Monad.Par as P
import Control.DeepSeq

-- type Ohua s a = a -> State [s] a
type SF s a = a -> State [s] a
type Ohua s a = a -> StateT [s] Par a

-- sf :: Int -> (a -> State s a) -> a -> State [s] a
-- sf :: Int -> SF s a -> SF [s] a
sf :: Int -> (a -> State s a) -> a -> State [s] a
sf i f d = do
  gs <- S.get
  let (x,y:ys) = splitAt i gs
  let (d', y') = runState (f d) y
  S.put $ x ++ y' : ys
  return d'

-- smap :: (a -> State s a) -> a -> State [s] a
-- smap :: SF s a -> SF s [a]
-- smap = mapM

-- infixr 9 .@.
-- (.@.) :: Ohua s a -> Ohua s a -> Ohua s a
-- sf .@. sg = \x -> (sg x) >>= sf

-- TODO this would make for a much nicer approach!
-- instance Monad (Ohua s a) where
--   return = Ohua s
--   p >>= q = undefined

data IList a = Null | Cons a (IVar (IList a))
type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
  rnf Null = ()
  rnf (Cons a as) = a `deepseq` rnf as

dff :: (NFData a, NFData b) => Stream a -> Stream b -> (a -> State s b) -> s -> Par s
-- dff :: Stream a -> Stream a -> Ohua s a -> StateT [s] Par a
-- dff :: Stream a -> Stream a -> SF [s] a -> Ohua s ()
dff input output f state = do
  i <- P.get input -- retrieve an element from the stream
  case i of
    Null -> do
      P.put output Null -- propagate the EOS and shut down
      return state
    Cons e es -> do
      newPendingTail <- P.new
      let (result,state') = runState (f e) state
      -- this is tricky! because for that the state would have to be a concurrent object.
      P.put output $ Cons result newPendingTail
      dff input newPendingTail f state'

type ParState s = s -> Par s

type SFb s b = StateT s Par b

myfork :: ParState s -> StateT s Par (StateT s Par s)
myfork f = do
  state0 <- get
  ivar <- lift $ spawn $ f state0
  return $ get >>= \gs -> P.get ivar >>= \ls -> put $ merge ls gs

infixr 9 .#.
-- (.#.) :: SF s b -> SF s a -> Ohua s b
(.#.) :: (a -> State ls b) -> StateT s Par (IVar a) -> a -> StateT gs Par (IVar b)
sf .#. sg = q where
  q :: a -> StateT s Par b
  q list@(_:_) = do
    state <- S.get
    [input,middle,output] <- lift $ sequence [new,new,new]
    stateUpdate1 <- myfork $ dff input middle sf
    stateUpdate2 <- myfork $ dff middle output sg
    pushInput list
    return $ pullOutput output []
      where
        pushInput :: [a] -> Stream a -> ()
        pushInput (x:xs) chan = do
          next <- lift $ new
          lift $ P.put chan $ Cons x next
          pushInput xs next
        pushInput [] chan = do
          lift $ P.put chan Null
        pullOutput :: Stream a -> [a] -> [a]
        pullOutput chan result = do
          i <- lift $ P.get chan
          case i of
            Null -> do
              stateUpdate1
              stateUpdate2
              result
            Cons e es -> pullOutput es $ result ++ [e]
  q x = (head . q) [x]

liftSf :: (a -> State s b) -> (a -> StateT s Par (IVar b))
liftSf f xs = do
  state <- S.get
  [input,output] <- lift $ sequence [new,new]
  stateUpdate <- myfork $ dff input output f
  pushInput xs
  return $ pullOutput output []
    where
      pushInput :: [a] -> Stream a -> ()
      pushInput (x:xs) chan = do
        next <- lift $ new
        lift $ P.put chan $ Cons x next
        pushInput xs next
      pushInput [] chan = do
        lift $ P.put chan Null
      pullOutput :: Stream a -> [a] -> [a]
      pullOutput chan result = do
        i <- lift $ P.get chan
        case i of
          Null -> do
            stateUpdate -- ???
            result
          Cons e es -> pullOutput es $ result ++ [e]

smap' :: StateT s Par b -> [a] -> s -> [b]
smap' f x gs = runPar $ runStateT $ f x gs

newtype OhuaM s a = OhuaM (Par (Stream a ,s -> Par s, s))

instance Monad (OhuaM s) where

  (>>=) :: OhuaM s a -> (a -> OhuaM s b) -> OhuaM s b
  OhuaM comp0 >>= f = OhuaM $ do
    (input, update, gs) <- comp0
    output <- new
    updatedState <- spawn $ dff input output f gs
    return
      ( output
      , \s -> do
          us <- update s
          s' <- P.get updatedState
          return $ merge s' us
      , gs
      )

liftWithIndex :: Int -> (a -> State s b) -> a -> OhuaM [s] b


s1 = liftWithIndex 5 $ \ x -> ....

-- OhuaM ..
-- do
--   r0 <- a x
--   r1 <- b x
--   r2 <- c x
--
--   <- smap
--
-- runOhua m s
