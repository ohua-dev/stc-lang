
import SBFMPerfBenchmark
import Control.Monad.State
import GHC.Stats

main = do
  _ <- coresTest [4] $ flip evalStateT (0::Int)
  -- stats <- getRTSStats
  -- putStrLn $ show stats
  return ()
