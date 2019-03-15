{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.DeepSeq
import Criterion
import Criterion.IO
import Criterion.Main
import Criterion.Internal
import Criterion.Types (verbosity, Verbosity(Quiet))
import Data.StateElement
import Data.Time.Clock.POSIX
import Data.Word
import Monad.FuturesBasedMonad
import System.IO (stdout)
import Control.Monad.State.Lazy (get, put)
import Control.Monad ((>=>))

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

{-# INLINE comp #-}
comp g coll= runOhuaM (smap (f >=> h) coll) [toS (4.0 :: Float), toS (3.0 :: Float)]
  where
    f = liftWithIndex 0 g
    h = liftWithIndex 1 g

{-# NOINLINE testData #-}
testData = take 50 $ iterate (+ 10.0) 400.0

cores = [1 .. 4]

main =
    defaultMainWith
        defaultConfig
        [ bench "sequential" (nf (map sequential) testData)
        , bench "pure" (nfIO $ comp expensiveComputation testData)
        , bench "reading" (nfIO $ comp readingExpensiveComputation testData)
        , bench "writing" (nfIO $ comp writingExpensiveComputation testData)
        ]
