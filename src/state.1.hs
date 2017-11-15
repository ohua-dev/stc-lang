
import Control.Monad.State


data S s = S { s1 :: s, s2 :: s, s3 :: s} deriving (Show)
 
type SF s a = a -> (State (S s) a)
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
  return $ x

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


main = let xs = [0..9]
           zero = 0 :: Integer
           s = S zero zero zero
       in do
        putStrLn . show $ runState (smap (f3.@.f2.@.f1) xs) s
        putStrLn . show $ runState (smap (f1.@.f3.@.f2) xs) s
        putStrLn . show $ runState (smap (f1.+.g1) xs) s
        putStrLn . show $ runState (smap (g1.+.f1) xs) s

 
