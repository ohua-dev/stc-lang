import BasicBench
import MatMult

import Criterion
import Criterion.Main

main =
  defaultMainWith
    defaultConfig
    [ohuaBenchmark, compBenchmark, appBenchmark, condBenchmark, matmultBench]
