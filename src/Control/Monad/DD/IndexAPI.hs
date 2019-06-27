{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.DD.IndexAPI where

import Control.Monad.DD
import Control.Monad.State
import Data.Dynamic2
import Lens.Micro
import Ohua.Types

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad.Stream

-- maps the StreamsBasedFreeMonad implementation to the explicit API.
sfConst' :: Typeable a => a -> ASTM s (Var a)
sfConst' a = call (liftSf (sfm $ pure a)) stag

arrayAccess gsIdx =
    lens
        (forceDynamic . (!! gsIdx))
        (\s n ->
             case splitAt gsIdx s of
                 (xs, _:ys) -> xs ++ [toDyn n] ++ ys)

liftWithIndexNamed ::
       forall s a b. (Typeable a, Typeable b, Typeable s, NFData b)
    => Int
    -> QualifiedBinding
    -> (a -> StateT s IO b)
    -> Var a
    -> ASTM [Dynamic] (Var b)
liftWithIndexNamed gsIdx name fn = call (liftSfNamed name f) $ arrayAccess gsIdx
  where
    f :: a -> SfMonad s b
    f = SfMonad . (\x -> fn x >>= (liftIO . evaluate . force))

lift2WithIndexNamed ::
       forall s a b c. (Typeable a, Typeable b, Typeable c, Typeable s)
    => Int
    -> QualifiedBinding
    -> (a -> b -> StateT s IO c)
    -> Var a
    -> Var b
    -> ASTM [Dynamic] (Var c)
lift2WithIndexNamed gsIdx name fn =
    call (liftSfNamed name f) $ arrayAccess gsIdx
  where
    f :: a -> b -> SfMonad s c
    f = (SfMonad .) . fn

lift3WithIndexNamed ::
       forall s a b c d.
       (Typeable a, Typeable b, Typeable c, Typeable d, Typeable s, NFData d)
    => Int
    -> QualifiedBinding
    -> (a -> b -> c -> StateT s IO d)
    -> Var a
    -> Var b
    -> Var c
    -> ASTM [Dynamic] (Var d)
lift3WithIndexNamed gsIdx name fn =
    call (liftSfNamed name f) $ arrayAccess gsIdx
  where
    f :: a -> b -> c -> SfMonad s d
    f x y = SfMonad . (\z -> fn x y z >>= (liftIO . evaluate . force))

lift4WithIndexNamed ::
       forall s a b c d e.
       (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable s)
    => Int
    -> QualifiedBinding
    -> (a -> b -> c -> d -> StateT s IO e)
    -> Var a
    -> Var b
    -> Var c
    -> Var d
    -> ASTM [Dynamic] (Var e)
lift4WithIndexNamed gsIdx name fn =
    call (liftSfNamed name f) $ arrayAccess gsIdx
  where
    f :: a -> b -> c -> d -> SfMonad s e
    f x y z = SfMonad . fn x y z

liftWithIndex ::
       forall s a b. (NFData b)
    => (Typeable a, Typeable b, Typeable s) =>
           Int -> (a -> StateT s IO b) -> Var a -> ASTM [Dynamic] (Var b)
liftWithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> SfMonad s b
    f = SfMonad . (\x -> fn x >>= (liftIO . evaluate . force))

lift2WithIndex ::
       forall s a b c. (Typeable a, Typeable b, Typeable c, Typeable s)
    => Int
    -> (a -> b -> StateT s IO c)
    -> Var a
    -> Var b
    -> ASTM [Dynamic] (Var c)
lift2WithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> b -> SfMonad s c
    f = (SfMonad .) . fn

lift3WithIndex ::
       forall s a b c d.
       (Typeable a, Typeable b, Typeable c, Typeable d, Typeable s)
    => Int
    -> (a -> b -> c -> StateT s IO d)
    -> Var a
    -> Var b
    -> Var c
    -> ASTM [Dynamic] (Var d)
lift3WithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> b -> c -> SfMonad s d
    f x y = SfMonad . fn x y

lift4WithIndex ::
       forall s a b c d e.
       (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable s)
    => Int
    -> (a -> b -> c -> d -> StateT s IO e)
    -> Var a
    -> Var b
    -> Var c
    -> Var d
    -> ASTM [Dynamic] (Var e)
lift4WithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> b -> c -> d -> SfMonad s e
    f x y z = SfMonad . fn x y z

runOhuaM :: (Typeable a, MonadStream m) => ASTM s (Var a) -> s -> m a
runOhuaM comp s = flip runAlgo s =<< liftIO (createAlgo comp)
