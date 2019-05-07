{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Criterion
import Criterion.IO
import Criterion.Types
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap as HM
import Data.List
import qualified Data.Vector as V
import Statistics.Types (confIntLDX, confIntUDX, estError, estPoint)
import System.Environment
import System.Exit
import System.IO
import System.Process as P

toScriptFormat :: [(Word, [Report])] -> Value
toScriptFormat = toJSON . concatMap mapRun
  where
    mapRun (cores, reps) =
      map
        (\rep ->
           object
             [ "config" .=
               object ["experiment" .= reportName rep, "cores" .= cores]
             -- , "data" .=
               -- toJSON
                 -- (reverse $
                 --  snd $
                 --  foldl
                 --    (\(old, acc) m ->
                 --       ( measTime m
                 --       , object
                 --           [ "start" .= old
                 --           , "finish" .= measTime m
                 --           , "iters" .= measIters m
                 --           ] :
                 --         acc))
                 --    (0.0, [])
                 --    (reportMeasured rep))
             , "mean" .=
               let meanEstimate = anMean $ reportAnalysis rep
                in object
                     [ "value" .= estPoint meanEstimate
                     , "upper" .= confIntLDX (estError meanEstimate)
                     , "lower" .= confIntUDX (estError meanEstimate)
                     ]
             ])
        reps

reportFileName = "results/reports.json"

runExperiment lo hi benchmark variants = do
  let fname = benchmark ++ "-" ++ show lo ++ ":" ++ show hi ++ "-results"
      runFor c name = do
        callProcess
          "./bin/benchmarks"
          [ "--json"
          , reportFileName
          , "--match"
          , "prefix"
          , benchmark ++ "/" ++ name
          , "+RTS"
          , "-N" ++ show (c :: Word)
          ]
        (_, _, recs) <- either error pure =<< readJSONReports reportFileName
        pure recs
  let multiVars = filter (/= "sequential") variants
  sequential <- runFor 1 "sequential"
  multiRecs <-
    forM [lo .. hi] $ \c -> do
      recs <- concat <$> mapM (runFor c) multiVars
      pure (c, recs)
  let recs = (1, sequential) : multiRecs
  -- writeFile ("results/" ++ fname ++ ".csv") $
  --   unlines $
  --   map
  --     (\(c, recs) ->
  --        intercalate "," $
  --        show c : map (show . avg) recs ++ map (show . outliers) recs)
  --     recs
  B.writeFile ("results/" ++ fname ++ ".json") $ encode $ toScriptFormat recs

-- forM ["ohua-bench", "comp-bench", "app-bench", "cond-bench"] $ \benchmark -> do
setUpExperiment = do
  callProcess "stack" ["build"]
  callProcess
    "stack"
    [ "ghc"
    , "--"
    , "src/benchmarks.hs"
    , "-o"
    , "./bin/benchmarks"
    , "-outputdir"
    , "./bin/"
    , "-rtsopts"
    , "-threaded"
    ]

experiments =
  [ ("ohua-bench", ["sequential", "pure", "reading", "writing"])
  , ("comp-bench", ["sequential", "ohua", "par1", "par2"])
  , ("app-bench", ["sequential", "ohua", "par"])
  , ("cond-bench", ["sequential", "ohua", "par"])
  , ("matmult-bench", ["sequential", "ohua", "par"])
  , ("bs-bench", ["sequential", "ohua", "par"])
  , ("mandel-bench", ["sequential", "ohua", "par"])
  , ("sumeuler-bench", ["sequential", "ohua", "par"])
  ]

runAllExperiments lo hi = do
  forM experiments $ \(name, variants) -> runExperiment lo hi name variants

main = do
  [name, cs] <- getArgs
  let (lo, hi) = read cs
  setUpExperiment
  if name == "all"
    then runAllExperiments lo hi
    else (\x -> return [x]) =<< (runExperiment lo hi name $ getVariants name)
  where
    getVariants benchmark =
      let variants = HM.lookup benchmark (HM.fromList experiments)
       in case variants of
            (Just v) -> v
            (Nothing) -> error $ "No such benchmark: " ++ benchmark
