{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simple where

import qualified Control.Monad.State
import Control.Monad.Trans.State

data S s = S { s1 :: s, s2 :: s, s3 :: s} deriving (Show)

-- type SF s a = a -> StateT (S s) IO a
type SF s a = a -> State (S s) a

foo :: Int -> State Int Int
foo x = do
  s <- Control.Monad.State.get
  Control.Monad.State.put $ s+2
  return $ x+2

bar :: Int -> State Int Int
bar x = do
  s <- Control.Monad.State.get
  Control.Monad.State.put $ s+3
  return $ x*3

sfc1 :: forall a s.(a -> State s a) -> a -> State (S s) a
sfc1 f x = q where
  f' = f x
  q' :: (S s) -> (a, S s)
  q' gs0 = (y, gs1) where
      S s1 s2 s3 = gs0
      (y, small) = runState f' s1
      gs1 = S small s2 s3
  q = state q'

sfc2 :: forall a s.(a -> State s a) -> a -> State (S s) a
sfc2 f x = q where
  f' = f x
  q' :: (S s) -> (a, S s)
  q' gs0 = (y, gs1) where
      S s1 s2 s3 = gs0
      (y, small) = runState f' s2
      gs1 = S s1 small s3
  q = state q'

infixr 9 .@.
(.@.) :: SF s a -> SF s a -> SF s a
sf .@. sg = \x ->
              (sg x) >>= sf

someFunc :: Int -> S Int -> (Int, S Int)
someFunc x = runState (((sfc2 bar) .@. (sfc1 foo)) x)

main :: IO ()
main = do
  print $ runState (((sfc2 bar) .@. (sfc1 foo)) 10) $ S 0 0 0
