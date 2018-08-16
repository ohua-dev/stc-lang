
import SBFMPerfBenchmark
import GHC.Stats

import Control.Monad.Stream.Par
import Control.Monad.Stream.Chan
import Control.Monad.State


main = do
  -- _ <- coresTest [4] $ flip evalStateT (0::Int) -- use: +RTS -qm -RTS to avoid thread migration
  -- _ <- coresTest [4] runChanM -- scales, but not as good as the above. don't use the -qm option!
  _ <- coresTest [4] runParIO -- scales as good as the first option!
  -- stats <- getRTSStats
  -- putStrLn $ show stats
  return ()
