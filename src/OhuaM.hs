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

liftWithIndex :: Int -> (a -> State s b) -> a -> OhuaM [s] b
sf i f d = do
  gs <- oget
  let (x,y:ys) = splitAt i gs
  let (d', y') = runState (f d) y
  oput $ x ++ y' : ys
  return d'

-- runOhuaM :: OhuaM s a -> s -> (a,s)
-- runOhuaM comp state =
--   let (output,updates,gs) = runPar $ comp s
--   let results = runPar $ collect output
--   let finalState = runPar updates
--   (results, finalState)

-- is this still just mapM? -> no because the key is to proceed to the next item
-- before a result was returned. this must be a recursive function that allows
-- for that. (sounds like map!)
smap :: (a -> OhuaM s b) -> [a] -> OhuaM s [b]
smap f xs = undefined

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
