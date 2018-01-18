{-# LANGUAGE MonadComprehensions #-}
module Generator where


import           System.Directory
import           System.FilePath


data Generator a
  = Finished
  | Yield (Generator a) a
  | NeedM (IO (Generator a))


instance Functor Generator where
  fmap _ Finished    = Finished
  fmap f (NeedM m)   = NeedM $ fmap (fmap f) m
  fmap f (Yield g a) = Yield (fmap f g) (f a)

instance Applicative Generator where
  pure = Yield Finished
  Finished <*> _ = Finished
  NeedM m <*> v = NeedM $ do
    f <- m
    pure $ f <*> v
  Yield fg f <*> v = recurse (fg <*> v) v
    where
      recurse cont m =
        case m of
          Finished   -> cont
          NeedM ac   -> NeedM $ again <$> ac
          Yield vg v -> Yield (again vg) (f v)
        where
          again = recurse cont

instance Monad Generator where
  return = pure
  Finished >>= _ = Finished
  Yield cont a >>= f = recurse (cont >>= f) (f a)
    where
      recurse cont m =
        case m of
          Finished    -> cont
          NeedM ac    -> NeedM $ again <$> ac
          Yield gen a -> Yield (again gen) a
        where
          again = recurse cont


listGenerator :: [a] -> Generator a
listGenerator []     = Finished
listGenerator (x:xs) = Yield (listGenerator xs) x


runGenerator :: Generator a -> IO [a]
runGenerator Finished    = pure []
runGenerator (NeedM ac)  = ac >>= runGenerator
runGenerator (Yield g a) = (a:) <$> runGenerator g

