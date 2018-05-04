{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse, OverloadedLists, MultiWayIf, OverloadedStrings #-}

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
import Data.Time
import Data.Aeson
import qualified Data.ByteString.Lazy as BS


bf_generate :: Int -> Int -> Graph2 -> Sf (SfMonad s (Generator IO Int))
bf_generate k0 startNode g =
    liftSf $
    sfm $
    pure $
    let gen seen_rank k new_rank
            | k == 0 = finish
            | IS.null new_rank = finish
            | otherwise = do
                let seen_rank' = IS.union seen_rank new_rank
                    allNbr' =
                        IS.fold
                            (\i acc -> IS.union (g V.! i) acc)
                            IS.empty
                            new_rank
                    new_rank' = IS.difference allNbr' seen_rank'
                (foldableGenerator (IS.toList new_rank') `mappend`
                 gen seen_rank' (pred k) new_rank')
     in gen mempty k0 [startNode]



forceA :: (Applicative m, NFData a) => a -> m a
forceA a = a `deepseq` pure a

start_traverse :: Starter
start_traverse k g startNode f = do
    algo <-
        createAlgo $ do
            nodeStream <- call (bf_generate k startNode g) united
            processedStream <-
                smapGen
                    (call
                         (liftSf $ const $ sfm $ liftIO getCurrentTime)
                         united)
                    nodeStream
            call
                (liftSf $ \gen -> sfm $
                     forceA =<< liftIO (G.toList gen))
                united
                processedStream
    begin <- getCurrentTime
    stamps <- runAlgo algo ()
    end <- getCurrentTime
    BS.writeFile "timestamps.json" $ encode $ object ["begin" .= begin, "stamps" .= stamps, "end" .= end]
    putStrLn "done with processing"
    --putStrLn $ "  * Set size: " ++ show (Set.size set)
    --putStrLn $ "  * Set sum: " ++ show (Set.foldr (\(x,_) y -> x+y) 0 set)

    


main = do
  makeMain start_traverse
