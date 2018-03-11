{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonadComprehensions    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Generator where


import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Monad.State
import           Data.Tuple

------------------------------------------------------------------
--
-- The generator
--
------------------------------------------------------------------

data Monad.Generator a
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

-- | This is needed to get the Monad comprehensions
instance Alternative Generator where
  empty = mempty
  (<|>) = mappend

-- | IO can be embedded easily
instance MonadIO Generator where
  liftIO = NeedM . fmap pure

-- | Run a generator producing a list of output values
runGenerator :: Generator a -> IO [a]
runGenerator Finished    = pure []
runGenerator (NeedM ac)  = ac >>= runGenerator
runGenerator (Yield g a) = (a:) <$> runGenerator g


-- | Run until the generator yields its first value or finishes.
-- Returns the created value and a new generator which represents its updated internal state.
runGeneratorOnce :: Generator a -> IO (Maybe (a, Generator a))
runGeneratorOnce Finished    = pure Nothing
runGeneratorOnce (NeedM ac)  = ac >>= runGeneratorOnce
runGeneratorOnce (Yield g a) = pure $ Just (a, g)


------------------------------------------------------------------
--
-- Creating generators
--
------------------------------------------------------------------



listGenerator :: [a] -> Generator a
listGenerator []     = Finished
listGenerator (x:xs) = Yield (listGenerator xs) x


-- | A generator crated with this will run until it returns `Nothing` in which case the generator finishes
stateToGenerator :: StateT s IO (Maybe a) -> s -> Generator a
stateToGenerator st s = NeedM $ do
  (a, s') <- runStateT st s
  pure $ maybe Finished (Yield (stateToGenerator st s')) a



------------------------------------------------------------------
--
-- Some more fun stuff that can be done with them
--
------------------------------------------------------------------

-- One fun thing we can do in IO is put the generator in a mutable variable and then just pull values from that.

type GenVar a = MVar (Generator a)

mkGenIO :: Generator a -> IO (GenVar a)
mkGenIO = newMVar

-- | This pulls a new value from this var (if possible) and updates its state
pull :: GenVar a -> IO (Maybe a)
pull = flip modifyMVar $ fmap (maybe (Finished, Nothing) (second Just . swap)) . runGeneratorOnce



------------------------------------------------------------------
--
-- Some examples of comprehensions, composition and state embedding
--
------------------------------------------------------------------


permutations :: Generator (Int, Char)
permutations =
  [ (i, c)
  | i <- listGenerator [0..9]
  , c <- listGenerator ['a'..'f']
  ]

nonReflexivePermutations :: Int -> Generator (Int, Int)
nonReflexivePermutations i =
  [ (a, b)
  | a <- ints
  , b <- ints
  , a /= b
  ]
  where ints = listGenerator [0..i]

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


-- and they are all compatible and can be joined together (and depend on each other)

-- Probably dont run this ... it creates a **lot** of output
crazy :: Generator (Int, Int, Char)
crazy =
  [ (a + b, a * d, c)
  | i <- justSomeStuffWithInts
  , (b, d) <- nonReflexivePermutations i
  , (a, c) <- permutations
  ]


------------------------------------------------------------------
--
-- A generator interface
--
------------------------------------------------------------------


-- Something I thought of this morning.
-- There could also be a generic interface for generators


-- | A generator @g@ that runs in the monad @m@
class Monad m => IsGenerator g m | g -> m where
  advance :: g a -> m (Maybe (a, g a))
  genToList :: g a -> m [a]
  genToList g = advance g >>= maybe (pure []) (\(a, ng) -> (a:) <$> genToList ng)


instance IsGenerator Generator IO where
  advance = runGeneratorOnce
  genToList = runGenerator
