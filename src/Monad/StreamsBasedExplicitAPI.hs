{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monad.StreamsBasedExplicitAPI where

import           Control.Monad.State
import           Data.Dynamic2
import           Data.Typeable
import           Lens.Micro
import           Monad.StreamsBasedFreeMonad


-- maps the StreamsBasedFreeMonad implementation to the explicit API.

sfConst' :: Typeable a => a -> ASTM s (Var a)
sfConst' a = call (liftSf (sfm $ pure a)) stag

arrayAccess gsIdx = lens (forceDynamic . (!! gsIdx))
                         (\s n -> case splitAt gsIdx s of
                                      (xs, _:ys) -> xs ++ [toDyn n] ++ ys)

liftWithIndex :: forall s a b.(Typeable a, Typeable b, Typeable s)
              => Int -> (a -> StateT s IO b) -> Var a -> ASTM [Dynamic] (Var b)
liftWithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> SfMonad s b
    f = SfMonad . fn

lift2WithIndex :: forall s a b c.(Typeable a, Typeable b, Typeable c, Typeable s)
               => Int -> (a -> b -> StateT s IO c) -> Var a -> Var b -> ASTM [Dynamic] (Var c)
lift2WithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> b -> SfMonad s c
    f = (SfMonad .) . fn

lift3WithIndex :: forall s a b c d.(Typeable a, Typeable b, Typeable c, Typeable d, Typeable s)
               => Int -> (a -> b -> c -> StateT s IO d) -> Var a -> Var b -> Var c -> ASTM [Dynamic] (Var d)
lift3WithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> b -> c -> SfMonad s d
    f x y = SfMonad . fn x y

lift4WithIndex :: forall s a b c d e.(Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable s)
               => Int -> (a -> b -> c -> d -> StateT s IO e) -> Var a -> Var b -> Var c -> Var d -> ASTM [Dynamic] (Var e)
lift4WithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> b -> c -> d -> SfMonad s e
    f x y z = SfMonad . fn x y z


runOhuaM :: (Typeable a) => ASTM s (Var a) -> s -> IO a
runOhuaM comp s = flip runAlgo s =<< createAlgo comp
