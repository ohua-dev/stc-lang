{-# LANGUAGE DeriveGeneric, ExistentialQuantification #-}
module BenchLib
    ( ExpName
    , Iterations
    , Record(..)
    , Result(..)
    , FinishedExperiment
    , Benchmarkable
    , Benchmark
    , bench
    , bgroup
    , nf
    , nfIO
    , setupTeardownBench
    , makeMain
    , encodeFile
    , decodeFile
    ) where

import Options.Applicative
import Data.Binary
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Monoid
import Data.List (isPrefixOf)
import System.Mem (performGC)
import System.Clock

type ExpName = String
type Iterations = Word

data Config = Config
    { selection :: [ExpName]
    -- , coreRange :: [Word]
    , iterations :: Iterations
    , outputFile :: FilePath
    } deriving (Show, Generic)
data Record = Record
    { measuredTime :: Word64
    } deriving (Generic, Show)
data Result = Result
    { name :: ExpName
    , data_ :: [Record]
    } deriving (Generic, Show)

type FinishedExperiment = (Config, [Result])
type Benchmarkable = Iterations -> IO [Record]
type Benchmark = [(ExpName, Benchmarkable)]

instance Binary Config
instance Binary Record
instance Binary Result

data StopWatch = StopWatch TimeSpec

stopWatchStdClock :: IO TimeSpec
stopWatchStdClock = getTime Monotonic

startStopWatch :: IO StopWatch
startStopWatch = StopWatch <$> stopWatchStdClock

measureStopWatch :: StopWatch -> IO Word64
measureStopWatch (StopWatch begin) = do
    time <- stopWatchStdClock
    let diff = diffTimeSpec time begin
        nanosecs = toNanoSecs diff
    pure $ fromIntegral nanosecs

setupTeardownBench :: IO a -> (a -> IO b) -> (a -> IO c) -> Benchmarkable
setupTeardownBench setup run cleanup iters = do
    a <- setup
    let measure = do
            performGC
            watch <- startStopWatch
            run a
            stopTime <- measureStopWatch watch
            pure $ Record stopTime
        go 0 = pure []
        go n = do
            r <- measure
            rs <- go (n - 1)
            pure $ r : rs
    results <- go iters
    cleanup a
    pure results

runBenches :: Config -> Benchmark -> IO [Result]
runBenches cfg =
    mapM (\(name, run) -> Result name <$> run (iterations cfg)) .
    filter (selector . fst)
  where
    selector
        | null (selection cfg) = const True
        | otherwise = \i -> any (`isPrefixOf` i) (selection cfg)

bench :: ExpName -> Benchmarkable -> Benchmark
bench name ac = [(name, ac)]

bgroup :: ExpName -> [Benchmark] -> Benchmark
bgroup prefix benches =
    [ (prefix ++ "/" ++ name, exp)
    | bench <- benches
    , (name, exp) <- bench
    ]

nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf f a = setupTeardownBench
    (f a `deepseq` pure ())
        (\() ->
             let x = f a
              in x `deepseq` pure ())
        (const $ pure ())

nfIO :: NFData b => IO b -> Benchmarkable
nfIO ac =
    setupTeardownBench
        (pure ())
        (\() -> do
             x <- ac
             x `deepseq` pure ())
        (const $ pure ())

makeMain :: [Benchmark] -> IO ()
makeMain bs = do
    cfg <- execParser $ info (helper <*> cfgParser) fullDesc
    res <- concat <$> mapM (runBenches cfg) bs
    encodeFile (outputFile cfg) (cfg, res)
  where
    cfgParser =
        Config <$>
        many
            (strOption
                 (long "select" <> short 's' <>
                  help "name of the benchmark(s) to select")) <*>
        -- many
        --     (option
        --          auto
        --          (long "core" <> short 'c' <>
        --           help "Number of cores to run the benchmark for")) <*>
        option
            auto
            (long "iterations" <> short 'i' <>
             help "Number of iterations to run the experiments for") <*>
        strOption
            (long "output-file" <> short 'o' <> value "results.bin" <>
             help "File to write the results to." <>
             showDefault)
