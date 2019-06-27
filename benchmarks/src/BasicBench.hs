{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module BasicBench
    ( ohuaBenchmark
    , compBenchmark
    , appBenchmark
    , condBenchmark
    ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Monad ((>=>))
import Control.Monad.SD
import Control.Monad.State.Lazy (get, put)
import Criterion
import Data.StateElement
import Data.Time.Clock.POSIX
import Data.Word

import Control.Monad.Par (runPar)
import Control.Monad.Par.Combinator (parMap, parMapM)

work = 100000

currentTimeMillis :: IO Integer
currentTimeMillis = round . (* 1000) <$> getPOSIXTime

sin_iter :: Word64 -> Float -> Float
sin_iter 0 x = x
sin_iter n !x = sin_iter (n - 1) (x + sin x)

-- Four versions: sequential (no ohua), stateless, stateful, only reading
{-# INLINE sequential #-}
sequential = sin_iter work . sin_iter work

{-# INLINE expensiveComputation #-}
expensiveComputation :: Float -> SFM Float Float
expensiveComputation = pure . sin_iter work

{-# INLINE readingExpensiveComputation #-}
readingExpensiveComputation :: Float -> SFM Float Float
readingExpensiveComputation i = do
    s <- get
    expensiveComputation $ i + s

{-# INLINE writingExpensiveComputation #-}
writingExpensiveComputation :: Float -> SFM Float Float
writingExpensiveComputation i = do
    s <- get
    r <- expensiveComputation $ i + s
    put r
    pure r

app x = (sin_iter work x, sin_iter work x)

condition = (`mod` 2) . (`div` 10) . round

cond x =
    if condition x == 0
        then sin_iter work x
        else sin_iter work x

--
-- Ohua
--
{-# INLINE compOhua #-}
compOhua g coll =
    runOhuaM (smap (f >=> h) coll) [toS (4.0 :: Float), toS (3.0 :: Float)]
  where
    f = liftWithIndex 0 g
    h = liftWithIndex 1 g

{-# INLINE appOhua #-}
appOhua g coll =
    runOhuaM
        (smap (\x -> (,) <$> f x <*> h x) coll)
        [toS (4.0 :: Float), toS (3.0 :: Float)]
  where
    f = liftWithIndex 0 g
    h = liftWithIndex 1 g

caseComp branch1 branch2 x = do
    c <- pure $ condition x
    o <- case_ c [(0, branch1 x), (1, branch2 x)]
    return o

{-# INLINE condOhua #-}
condOhua g coll =
    runOhuaM (smap (caseComp f h) coll) [toS (4.0 :: Float), toS (3.0 :: Float)]
  where
    f = liftWithIndex 0 g
    h = liftWithIndex 1 g

--
-- Monad Par
--
{-# INLINE compPar1 #-}
compPar1 g coll = runPar $ (parMap g >=> parMap g) coll

{-# INLINE compPar2 #-}
compPar2 g coll = runPar $ (parMap $ g . g) coll

{-# INLINE appPar #-}
-- appPar g coll = runPar $ (,) <$> parMap g coll <*> parMap g coll
-- appPar :: (a -> a) -> [a] -> [(a, a)]
appPar g coll = runPar $ parMapM (\x -> (,) <$> pure (g x) <*> pure (g x)) coll

{-# INLINE condPar #-}
condPar g coll =
    runPar $
    parMap
        (\x ->
             if condition x == 0
                 then g x
                 else g x)
        coll

--
-- Strategies
--
{-# NOINLINE testData #-}
testData = take 50 $ iterate (+ 10.0) 400.0

cores = [1 .. 4]

ohuaBenchmark =
    bgroup
        "ohua-bench"
        [ bench "sequential" (nf (map sequential) testData)
        , bench "pure" (nfIO $ compOhua expensiveComputation testData)
        , bench "reading" (nfIO $ compOhua readingExpensiveComputation testData)
        , bench "writing" (nfIO $ compOhua writingExpensiveComputation testData)
        ]

compBenchmark =
    bgroup
        "comp-bench"
        [ bench "sequential" (nf (map sequential) testData)
        , bench "ohua" (nfIO $ compOhua expensiveComputation testData)
        , bench "par1" (nf (compPar1 $ sin_iter work) testData)
        , bench "par2" (nf (compPar2 $ sin_iter work) testData)
        ]

appBenchmark =
    bgroup
        "app-bench"
        [ bench "sequential" (nf (map app) testData)
        , bench "ohua" (nfIO $ appOhua expensiveComputation testData)
        , bench "par" (nf (appPar $ sin_iter work) testData)
        ]

condBenchmark =
    bgroup
        "cond-bench"
        [ bench "sequential" (nf (map cond) testData)
        , bench "ohua" (nfIO $ condOhua expensiveComputation testData)
        , bench "par" (nf (condPar $ sin_iter work) testData)
        ]
