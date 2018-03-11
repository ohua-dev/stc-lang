{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ExplicitForAll             #-}

module Monad.StreamsBasedExplicitAPI where

import           Monad.StreamsBasedFreeMonad
import           Control.Monad.State
import           Lens.Micro
import           Lens.Micro.Mtl
import           Data.Typeable
import           Data.Dynamic2


-- maps the StreamsBasedFreeMonad implementation to the explicit API.

sfConst' :: Typeable a => a -> ASTM s (Var a)
sfConst' a = call (liftSf (sfm $ pure a)) stag

arrayAccess gsIdx = lens (forceDynamic . (!! gsIdx))
                         (\s n -> let (xs, y:ys) = splitAt (gsIdx-1) s in xs ++ [toDyn n] ++ ys)

liftWithIndex :: forall s a b.(Typeable a, Typeable b, Typeable s)
              => Int -> (a -> StateT s IO b) -> Var a -> ASTM [Dynamic] (Var b)
liftWithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> SfMonad s b
    f = SfMonad . fn

lift3WithIndex :: forall s a b c.(Typeable a, Typeable b, Typeable c, Typeable s)
               => Int -> (a -> b -> StateT s IO c) -> Var a -> Var b -> ASTM [Dynamic] (Var c)
lift3WithIndex gsIdx fn = call (liftSf f) $ arrayAccess gsIdx
  where
    f :: a -> b -> SfMonad s c
    f x = SfMonad . fn x

runOhuaM :: (Typeable a) => ASTM s (Var a) -> s -> IO a
runOhuaM comp s = flip runAlgo s =<< createAlgo comp
