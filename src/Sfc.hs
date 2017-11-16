
module Sfc where

import Control.Monad.State
import Statefulness
import Foreign.Ptr
import Foreign
import System.IO.Unsafe

data S s = S { s1 :: s, s2 :: s, s3 :: s } deriving (Show)

type SF s a = a -> State (S s) a
--           (a -> (s -> (a,s)))


f1 :: (Num s, Num a) => SF s a
f1 x = do
  S s1 s2 s3 <- get
  put $ S (s1+2) s2 s3
  return $ x+2

f2 :: (Num s, Num a) => SF s a
f2 x = do
  S s1 s2 s3 <- get
  put $ S s1 (s2+3) s3
  return $ x*3

f3 :: (Num s, Num a) => SF s a
f3 x = do
  S s1 s2 s3 <- get
  put $ S s1 s2 (s3+5)
  return x

execFF :: (Ptr Result -> IO Int) -> (Int, Int)
execFF f = unsafePerformIO $
    alloca $ \resultPtr -> do
        err <- f resultPtr
        (Result v st) <- peek resultPtr
        return (convert v, convert st)
--        return $ (,) <$> convert val <*> convert state
          where convert s = fromIntegral (s :: Int32) :: Int

f1' :: SF Int Int
f1' x = do
  S s1 s2 s3 <- get
  let (v, st) = execFF $ c_f1 x s1
  put $ S st s2 s3
  return v

f2' :: SF Int Int
f2' x = do
  S s1 s2 s3 <- get
  let (v, st) = execFF $ c_f2 x s2
  put $ S s1 st s3
  return v

f3' :: SF Int Int
f3' x = do
  S s1 s2 s3 <- get
  let (v, st) = execFF $ c_f3 x s3
  put $ S s1 s2 st
  return v

g1 :: (Num s, Num a) => SF s a
g1 x = do
  S s1 s2 s3 <- get
  put $ S (s1*2) s2 s3
  return $ x+2


infixr 9 .@.
(.@.) :: SF s a -> SF s a -> SF s a
sf .@. sg = \x ->
              (sg x) >>= sf


infixl 6 .+.
(.+.) :: (Num a) => SF s a -> SF s a -> SF s a
sf .+. sg = \x -> do
              y1 <- sf x
              y2 <- sg x
              return $ y1+y2



smap :: SF s a -> SF s [a]
--     (a -> (s -> (a,s))) -> ([a] -> (s -> ([a],s))
smap f xs = forM xs f


--main = let xs = [0..9]
--           zero = 0 :: Integer
--           s = S zero zero zero
--       in do
--        putStrLn . show $ runState (smap (f3.@.f2.@.f1) xs) s
--        putStrLn . show $ runState (smap (f1.@.f3.@.f2) xs) s
--        putStrLn . show $ runState (smap (f1.+.g1) xs) s
--        putStrLn . show $ runState (smap (g1.+.f1) xs) s
