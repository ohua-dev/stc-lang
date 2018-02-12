

module StreamsBasedExplicitAPI where

import           StreamsBasedFreeMonad
import           Control.Monad.State
import           Lens.Micro
import           Lens.Micro.Mtl
import           Data.Typeable


-- maps the StreamsBasedFreeMonad implementation to the explicit API.

sfConst' :: Typeable a => a -> ASTM s (Var a)
sfConst' a = call (liftSf (sfm $ pure a)) stag

liftWithIndex :: (Typeable a, Typeable b) => Int -> (a -> SfMonad s b) -> Var a -> ASTM [s] (Var b)
liftWithIndex gsIdx fn = call (liftSf fn) $ lens (!! gsIdx) (\s n -> let (xs, y:ys) = splitAt (gsIdx-1) s in xs ++ [n] ++ ys)

lift3WithIndex :: (Typeable a, Typeable b, Typeable c) => Int -> (a -> b -> SfMonad s c) -> Var a -> Var b -> ASTM [s] (Var c)
lift3WithIndex gsIdx fn = call (liftSf fn) $ lens (!! gsIdx) (\s n -> let (xs, y:ys) = splitAt (gsIdx-1) s in xs ++ [n] ++ ys)

runOhuaM :: (Typeable a) => ASTM s (Var a) -> s -> IO a
runOhuaM comp s = flip runAlgo s =<< createAlgo comp
