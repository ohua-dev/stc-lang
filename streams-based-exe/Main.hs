

import           StreamsBasedFreeMonad


main = print =<< flip runAlgo 0 =<< algorithm
