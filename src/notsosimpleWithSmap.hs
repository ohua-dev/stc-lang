{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simple where

import Control.Monad.State hiding (state)
import Control.Monad.Trans.State (state)
import Data.List

data S s = S { s1 :: s, s2 :: s, s3 :: s} deriving (Show)

-- type SF s a = a -> StateT (S s) IO a
type SF s a = a -> State [s] a

foo :: Int -> State Int Int
foo x = do
  s <- get
  put $ s+2
  return $ x+2

bar :: Int -> State Int Int
bar x = do
  s <- get
  put $ s+3
  return $ x*3

sf :: Int -> (a -> State s a) -> a -> State [s] a
sf i f d = do
  gs <- get
  let (x,y:ys) = splitAt i gs
  let (d', y') = runState (f d) y
  put $ x ++ y' : ys
  return d'

infixr 9 .@.
(.@.) :: SF s a -> SF s a -> SF s a
sf .@. sg = \x ->
              (sg x) >>= sf

smap :: SF s a -> SF s [a]
smap = mapM

someFunc :: Int -> [Int] -> (Int, [Int])
someFunc x = runState (((sf 1 bar) .@. (sf 0 foo)) x)

smapTest :: [Int] -> IO ()
smapTest xs = do
  print $ runState (smap ((sf 1 bar) .@. (sf 0 foo)) xs) [0,0]
