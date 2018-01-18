{-# LANGUAGE MonadComprehensions #-}
module Generator where


import           Control.Applicative
import           Control.Monad.State


data Generator a
  = Finished
  | Yield (Generator a) a
  | NeedM (IO (Generator a))


-- | Smart constructor for `Yield`
yield :: a -> Generator a -> Generator a
yield = flip Yield


-- At first I implemented all of the `Applicative` and `Monad` functions fully,
-- that is to say with all the recursion down the source generator.
-- I was already suspecting a pattern there, because the recursion for both looked very similar,
-- I just couldn't quite figure it out.
-- When suddenly something *magical* happened.
-- I was looking at `Alternative`, because that instance is needed for `MonadComprehensions`
-- and I was again reminded of just how similar `Alternative` is to `Monoid` and I realized that
-- `Generator` is in fact a `Monoid`. The empty element is `Finished` because it can be appended or prepended to
-- any generator without changing its meaning and `mappend` is simply exhausting the first generator first,
-- followed by the second one.
-- After having that realization implementing `Monad` and `Applicative` became really easy because you just simply
-- create a new generator by applying the function and then prepend a recursion of the respective operation (`>>=` or `<*>`)

-- You can see the generator in action by runnning the examples at the bottom in ghci with `runGenerator`



instance Monoid (Generator a) where
  mempty = Finished
  Finished `mappend` gen2 = gen2
  NeedM sc `mappend` gen2 = NeedM $ (`mappend` gen2) <$> sc
  Yield g v `mappend` gen2 = Yield (g `mappend` gen2) v


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
  Yield fg f <*> v = fmap f v `mappend` (fg <*> v)

instance Monad Generator where
  return = pure
  Finished >>= _ = Finished
  NeedM m >>= f = NeedM $ (>>= f) <$> m
  Yield cont a >>= f = f a `mappend` (cont >>= f)

instance Alternative Generator where
  empty = mempty
  (<|>) = mappend

instance MonadIO Generator where
  liftIO = NeedM . fmap pure


listGenerator :: [a] -> Generator a
listGenerator []     = Finished
listGenerator (x:xs) = Yield (listGenerator xs) x


-- | Run a generator producing a list of output values
runGenerator :: Generator a -> IO [a]
runGenerator Finished    = pure []
runGenerator (NeedM ac)  = ac >>= runGenerator
runGenerator (Yield g a) = (a:) <$> runGenerator g


-- | A generator crated with this will run until it returns `Nothing` in which case the generator finishes
stateToGenerator :: StateT s IO (Maybe a) -> s -> Generator a
stateToGenerator st s = NeedM $ do
  (a, s') <- runStateT st s
  pure $ maybe Finished (Yield (stateToGenerator st s')) a


permutations :: Generator (Int, Char)
permutations =
  [ (i, c)
  | i <- listGenerator [0..9]
  , c <- listGenerator ['a'..'f']
  ]

nonReflexivePermutations :: Generator (Int, Int)
nonReflexivePermutations =
  [ (a, b)
  | a <- ints
  , b <- ints
  , a /= b
  ]
  where ints = listGenerator [0..9]

justSomeStuffWithInts :: Generator Int
justSomeStuffWithInts = flip stateToGenerator 0 $ do
  s <- get
  if s < 100
    then do
      modify (+ 4)
      pure $ Just s
    else do
      liftIO $ putStrLn "We have reached 100" -- It can do IO as well ;)
      pure Nothing
