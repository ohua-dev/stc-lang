module SumEuler
  ( sumEulerBench
  ) where

import Control.Monad.Par

{-Code taken from: https://github.com/simonmar/monad-par/blob/master/examples/src/sumeuler/sumeuler.hs -}
import qualified Control.Monad.Par.Combinator as C

import Control.Monad.SD as Ohua
import Criterion
import Criterion.Main

sumEuler_ohua :: Int -> Int -> IO Int
sumEuler_ohua _ n = Ohua.mapReduce euler (+) 0 [n,n - 1 .. 0]

sumEuler_ohua_sum :: Int -> Int -> IO Int
sumEuler_ohua_sum _ n = do
  (r, _) <- runOhuaM (sum <$> smap (pure . euler) [n,n - 1 .. 0]) []
  return r

sumEuler_monadpar :: Int -> Int -> Int
sumEuler_monadpar _ n = runPar $ sum `fmap` C.parMap euler [n,n - 1 .. 0]

--   sum `fmap` parMap (sum . map euler) (splitAtN c [n,n-1..0])
-- boring sequential version
sumEuler_seq :: Int -> Int
sumEuler_seq = sum . map euler . mkList

---------------------------------------------------------------------------
-- smallest input for euler
base :: Int
base = 0

-- produce a list of input values
mkList :: Int -> [Int]
mkList = reverse . enumFromTo base . (+ base)

-- random numbers
-- mkList seed n = take n (randoms seed)
---------------------------------------------------------------------------
-- main fct
euler :: Int -> Int
euler n = length (filter (relprime n) [1 .. (n - 1)])

---------------------------------------------------------------------------
-- orig code from Nathan
{-
euler :: Int -> Int
euler n = let
            relPrimes = let
                          numbers = [1..(n-1)]
                        in
                          numbers `par` (filter (relprime n) numbers)
          in
            (spine relPrimes) `par` (length relPrimes)
-}
---------------------------------------------------------------------------
-- aux fcts
hcf :: Int -> Int -> Int
hcf x 0 = x
hcf x y = hcf y (rem x y)

relprime :: Int -> Int -> Bool
relprime x y = hcf x y == 1

sumEulerBench =
  bgroup
    "sumeuler-bench"
    [ bench "sequential" (nf sumEuler_seq n)
    , bench "ohua" (nfIO $ sumEuler_ohua c n)
    , bench "sum_ohua" (nfIO $ sumEuler_ohua_sum c n)
    , bench "par" (nf (sumEuler_monadpar c) n)
    ]
    {- Values taken from: https://github.com/simonmar/monad-par/blob/master/examples/src/run_benchmark.hs-}
    -- desktop values
  where
    c = 100
    n = 8000
    -- server values
    -- c = 100
    -- n = 16000
