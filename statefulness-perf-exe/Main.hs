
import SBFMPerfBenchmark
import GHC.Stats

import Control.Monad.Stream.Par
import Control.Monad.Stream.Chan
import Control.Monad.State


main = do
  _ <- coresTest [4] $ flip evalStateT (0::Int)
  -- _ <- coresTest [4] runChanM
  -- _ <- coresTest [4] runParIO
  -- stats <- getRTSStats
  -- putStrLn $ show stats
  return ()
