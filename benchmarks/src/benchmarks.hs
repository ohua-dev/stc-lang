import BasicBench
import BlackScholes
import MatMult

import Criterion
import Criterion.Main

main =
  defaultMainWith
    defaultConfig
    [ ohuaBenchmark
    , compBenchmark
    , appBenchmark
    , condBenchmark
    , matmultBench
    , bsBench
    ]
