{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE CPP #-}

module Monad.Generator
    ( IsGenerator(..)
    , liftIO
    , foldlGenerator
    , foldlGeneratorT
    , foldlGenerator_
    , foldlGeneratorT_
    , chanToGenerator
    , ioReaderToGenerator
    , foldableGenerator
    , foldableGenerator'
    , foldableGenerator''
    , foldableGeneratorEval
    , listGenerator
    , stateToGenerator
    , Generator
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad.State
import Control.Natural
import qualified Data.Foldable as F
import Data.Foldable (foldr')
import Data.Tuple
import qualified GHC.Exts (IsList(..))

------------------------------------------------------------------
--
-- The generator
--
------------------------------------------------------------------
-- | There are three basic ways to construct this generator, which
-- correspond to the (not exposed) constructors of this
-- type. 1. 'finish' this marks the end of a generator, 2. 'yield' is
-- used to return a value and continue with the computation afterwards
-- and finally using 'liftIO' you can execute any IO action and then
-- continue.
data Generator m a
    = Finished
    | Yield (Generator m a)
            a
    | NeedM (m (Generator m a))

-- At first I implemented all of the `Applicative` and `Monad`
-- functions fully, that is to say with all the recursion down the
-- source generator.  I was already suspecting a pattern there,
-- because the recursion for both looked very similar, I just couldn't
-- quite figure it out.  When suddenly something *magical* happened.
-- I was looking at `Alternative`, because that instance is needed for
-- `MonadComprehensions` and I was again reminded of just how similar
-- `Alternative` is to `Monoid` and I realized that `Generator` is in
-- fact a `Monoid`. The empty element is `Finished` because it can be
-- appended or prepended to any generator without changing its meaning
-- and `mappend` is simply exhausting the first generator first,
-- followed by the second one.  After having that realization
-- implementing `Monad` and `Applicative` became really easy because
-- you just simply create a new generator by applying the function and
-- then prepend a recursion of the respective operation (`>>=` or
-- `<*>`)
-- You can see the generator in action by running the examples at the bottom in ghci with `runGenerator`
mappendGen :: Functor m => Generator m a -> Generator m a -> Generator m a
Finished `mappendGen` gen2 = gen2
NeedM sc `mappendGen` gen2 = NeedM $ (`mappendGen` gen2) <$> sc
Yield g v `mappendGen` gen2 = Yield (g `mappendGen` gen2) v

instance Functor m => Monoid (Generator m a) where
    mempty = Finished
#if MIN_VERSION_base(4,11,0)
instance Functor m => Semigroup (Generator m a) where
    (<>) = mappendGen
#else
    mappend = mappendGen
#endif
instance Functor m => Functor (Generator m) where
    fmap _ Finished = Finished
    fmap f (NeedM m) = NeedM $ fmap (fmap f) m
    fmap f (Yield g a) = Yield (fmap f g) (f a)

instance Functor m => Applicative (Generator m) where
    pure = Yield Finished
    Finished <*> _ = Finished
    NeedM m <*> v = NeedM $ (<*> v) <$> m
    Yield fg f <*> v = fmap f v `mappend` (fg <*> v)

instance Functor m => Monad (Generator m) where
    return = pure
    Finished >>= _ = Finished
    NeedM m >>= f = NeedM $ (>>= f) <$> m
    Yield cont a >>= f = f a `mappend` (cont >>= f)

-- | This is needed to get the Monad comprehensions
instance Functor m => Alternative (Generator m) where
    empty = mempty
    (<|>) = mappend

-- | IO can be embedded easily
instance MonadIO m => MonadIO (Generator m) where
    liftIO = needM . liftIO

instance Monad m => GHC.Exts.IsList (Generator m a) where
    type Item (Generator m a) = a
    fromList = listGenerator
    toList _ = error "toList: need monad to evaluate generator"

foldlGeneratorT ::
       (IsGenerator g f, Monad m)
    => (f ~> m)
    -> (b -> a -> m b)
    -> b
    -> g a
    -> m b
foldlGeneratorT trans ac = flip go
  where
    go gen seed' =
        trans (step gen) >>=
        maybe (pure seed') (\(a, gen') -> go gen' =<< ac seed' a)

foldlGenerator ::
       (IsGenerator g m, Monad m) => (b -> a -> m b) -> b -> g a -> m b
foldlGenerator = foldlGeneratorT id

foldlGeneratorT_ ::
       (IsGenerator g f, Monad m) => (f ~> m) -> (a -> m ()) -> g a -> m ()
foldlGeneratorT_ trans f = foldlGeneratorT trans (\() a -> f a) ()

foldlGenerator_ :: (IsGenerator g m, Monad m) => (a -> m ()) -> g a -> m ()
foldlGenerator_ = foldlGeneratorT_ id

ioReaderToGenerator :: (IsGenerator g m, Monad g) => (m (Maybe a)) -> g a
ioReaderToGenerator reader = recur
  where
    recur = maybe finish (`yield` recur) =<< needM reader

chanToGenerator ::
       (MonadIO m, IsGenerator g m, Monad g) => Chan (Maybe a) -> g a
chanToGenerator = ioReaderToGenerator . liftIO . readChan

foldableGenerator :: (Foldable f, IsGenerator g m) => f a -> g a
foldableGenerator = foldr' yield finish

foldableGeneratorEval ::
       (Foldable f, IsGenerator g m) => (forall b. a -> b -> b) -> f a -> g a
foldableGeneratorEval eval = foldr (\a rest -> a `eval` yield a rest) finish

foldableGenerator' :: (Foldable f, IsGenerator g m) => f a -> g a
foldableGenerator' = foldableGeneratorEval seq

foldableGenerator'' :: (Foldable f, IsGenerator g m, NFData a) => f a -> g a
foldableGenerator'' = foldableGeneratorEval deepseq

-----------------------------------------------------------------
--
-- Creating generators
--
------------------------------------------------------------------
listGenerator :: IsGenerator g m => [a] -> g a
listGenerator = foldableGenerator

-- | A generator crated with this will run until it returns `Nothing` in which case the generator finishes
stateToGenerator ::
       (Monad g, IsGenerator g m) => StateT s m (Maybe a) -> s -> g a
stateToGenerator st s = do
    (a, s') <- needM $ runStateT st s
    maybe finish (`yield` stateToGenerator st s') a

------------------------------------------------------------------
--
-- Some more fun stuff that can be done with them
--
------------------------------------------------------------------
-- One fun thing we can do in IO is put the generator in a mutable variable and then just pull values from that.
type GenVar a = MVar (Generator IO a)

mkGenIO :: Generator IO a -> IO (GenVar a)
mkGenIO = newMVar

-- | This pulls a new value from this var (if possible) and updates its state
pull :: GenVar a -> IO (Maybe a)
pull =
    flip modifyMVar $
    fmap (maybe (Finished, Nothing) (second Just . swap)) . step

------------------------------------------------------------------
--
-- Some examples of comprehensions, composition and state embedding
--
------------------------------------------------------------------
permutations :: Generator IO (Int, Char)
permutations =
    [(i, c) | i <- listGenerator [0 .. 9], c <- listGenerator ['a' .. 'f']]

nonReflexivePermutations :: Int -> Generator IO (Int, Int)
nonReflexivePermutations i = [(a, b) | a <- ints, b <- ints, a /= b]
  where
    ints = listGenerator [0 .. i]

justSomeStuffWithInts :: Generator IO Int
justSomeStuffWithInts =
    flip stateToGenerator 0 $ do
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
crazy :: Generator IO (Int, Int, Char)
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
class IsGenerator g m | g -> m where
    yield :: a -> g a -> g a
    finish :: g a
    needM :: m a -> g a
    isFinished :: g a -> Bool
    default isFinished :: Eq (g a) =>
        g a -> Bool
    isFinished = (== finish)
    -- | Run until the generator yields its first value or finishes.
    -- Returns the created value and a new generator which represents its updated internal state.
    step :: g a -> m (Maybe (a, g a))
    -- | Run a generator producing a list of output values
    toList :: g a -> m [a]
    default toList :: Monad m =>
        g a -> m [a]
    toList = foldlGenerator (\b a -> pure $ a : b) []

instance Monad m => IsGenerator (Generator m) m where
    yield = flip Yield
    finish = Finished
    needM = NeedM . fmap pure
    isFinished Finished = True
    isFinished _ = False
    step Finished = pure Nothing
    step (NeedM ac) = ac >>= step
    step (Yield g a) = pure $ Just (a, g)
    toList Finished = pure []
    toList (NeedM ac) = ac >>= toList
    toList (Yield g a) = (a :) <$> toList g
