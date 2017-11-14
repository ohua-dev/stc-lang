
import Control.Monad.State


data S s = S { s1 :: s, s2 :: s, s3 :: s} deriving (Show)
 
type SF s a b = a -> (State (S s) b)
--             (a -> (s -> (b,s)))


instance (Monoid s) => Monoid (S s) where
  mempty = S mempty mempty mempty
  mappend (S s1 s2 s3) (S t1 t2 t3) = let u1 = mappend s1 t1
                                          u2 = mappend s2 t2
                                          u3 = mappend s3 t3
                                      in S u1 u2 u3
                                      

f1 :: (Num s, Num a) => SF s a a
f1 x = do
  S s1 s2 s3 <- get
  put $ S (s1+2) s2 s3
  return $ x+2
  
f2 :: (Num s, Num a) => SF s a a
f2 x = do
  S s1 s2 s3 <- get
  put $ S s1 (s2+3) s3
  return $ x*3
  
f3 :: (Num s, Num a) => SF s a a
f3 x = do
  S s1 s2 s3 <- get
  put $ S s1 s2 (s3+5)
  return $ x

g1 :: (Num s, Num a) => SF s a a
g1 x = do
  S s1 s2 s3 <- get
  put $ S (s1*2) s2 s3
  return $ x+2
  

infixr 8 .@.
(.@.) :: SF s b c -> SF s a b -> SF s a c
(sf .@. sg) a = (sg a) >>= sf


infixl 5 .+.
(.+.) :: (Num a, Num b) => SF s a b -> SF s a b -> SF s a b
sf .+. sg = \x -> do
              y1 <- sf x
              y2 <- sg x
              return $ y1+y2

infixr 9 .^.
(.^.) :: (b -> c) -> SF s a b -> SF s a c
f .^. sg = \x -> do
              y <- sg x
              return $ f y


smap :: SF s a b -> SF s [a] [b]
--     (a -> (s -> (b,s))) -> ([a] -> (s -> ([b],s))
smap f xs = forM xs f


xs = [0..9] :: [Integer]
zero = 0 :: Integer
s = S zero zero zero
stringify = (\x -> "a" ++ (show x)) :: Integer -> String
suffix = (\a -> a ++ "__") 
add = (+) (1000 :: Integer)
algo = smap (f3.@.f2.@.f1)
p = let d = algo 
        l1 = [1, 2] :: [Integer]
        l2 = [3, 4] :: [Integer]
        x = d l1
        y = d l2
    in [x,y]


instance Monoid Integer where
  mempty  = 0
  mappend = (+)
  
  
main = do
  putStrLn . show $ runState (smap (f3.@.f2.@.f1) xs) s
  putStrLn . show $ runState (mmap  (f3.@.f2.@.f1) xs) s
  putStrLn . show $ runState (mmap' (f3.@.f2.@.f1) xs) s
  putStrLn . show $ runState (smap (f3.@.f2.@.add.^.f1) xs) s
  putStrLn . show $ runState (smap (stringify.^.f1.@.f3.@.f2) xs) s
  putStrLn . show $ runState (smap (f1.+.g1) xs) s
  putStrLn . show $ runState (smap (g1.+.f1) xs) s
  putStrLn . show $ runState (smap (stringify.^.(g1.+.f1)) xs) s
  putStrLn . show $ runState (smap (suffix.^.stringify.^.f3) xs) s



mmap :: (Monoid s) => SF s a b -> SF s [a] [b]
mmap f (x:xs) = state (\_ -> let (y,s)   = runState (f x) mempty
                                 (ys,s') = runState ((mmap f) xs) s
                             in (y:ys, s'))
mmap f [] = state (\s -> ([], s)) 


mmap' :: (Monoid s) => SF s a b -> SF s [a] [b]
mmap' f (x:xs) = do
  put mempty
  y  <- f x
  ys <- mmap' f xs
  return (y:ys)
mmap' f [] = return [] 

 
