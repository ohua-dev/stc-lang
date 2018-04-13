{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse, OverloadedLists #-}

module Main where

import Monad.Generator as G
import Runner
import qualified Data.IntSet as IS
import qualified Data.Set as Set 
import Control.DeepSeq
import Monad.StreamsBasedFreeMonad
import Control.Monad.Trans.Class
import Control.Monad.State
import qualified Data.Vector as V


bf_generate :: Int -> Sf (Graph2 -> SfMonad s (Generator IO Int))
bf_generate startNode =
    liftSf $ \gr ->
        sfm $
        pure $
        flip evalStateT (mempty, [startNode]) $ do
            (seen, last) <- get
            let discoveredNeighbours =
                    IS.fold (\i acc -> IS.union (gr V.! i) acc) IS.empty last
                new = IS.difference discoveredNeighbours seen
            put (IS.union seen new, new)
            lift $
                if IS.null new
                    then finish
                    else foldableGenerator $ IS.toList new

forceA :: (Applicative m, NFData a) => a -> m a
forceA a = a `deepseq` pure a

start_traverse :: Starter
start_traverse k g startNode f = do
    algo <-
        createAlgo $ do
            gr' <- sfConst g
            nodeStream <- call (bf_generate startNode) united gr'
            processedStream <-
                smapGen
                    (call
                         (liftSf $ \i -> sfm $ forceA $ f i)
                         united)
                    nodeStream
            call
                (liftSf $ \gen -> sfm $
                     forceA =<< (Set.fromList <$> liftIO (G.toList gen)))
                united
                processedStream
    set <- runAlgo algo ()
    putStrLn "done with processing"
    putStrLn $ "  * Set size: " ++ show (Set.size set)
    putStrLn $ "  * Set sum: " ++ show (Set.foldr (\(x,_) y -> x+y) 0 set)

    


main = do
  makeMain start_traverse
