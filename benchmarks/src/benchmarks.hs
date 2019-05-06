import BasicBench
import BlackScholes
import Mandelbrot
import MatMult
import SumEuler
import BenchLib


main = makeMain
    [ ohuaBenchmark
    , compBenchmark
    , appBenchmark
    , condBenchmark
    , matmultBench
    , bsBench
    , mandelBench
    , sumEulerBench
    ]
