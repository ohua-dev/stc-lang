import Control.Monad.DD.StreamsBasedFreeMonad
import Control.Monad.Stream.Chan

main = print =<< runChanM . flip runAlgo 0 =<< algorithm
