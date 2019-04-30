{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Criterion
import Criterion.IO
import Criterion.Types
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Data.Vector as V
import System.Environment
import System.Exit
import System.IO
import System.Process as P

toScriptFormat :: [(Word, [Report])] -> Value
toScriptFormat = toJSON . concatMap mapRun
  where
    mapRun (cores, reps) =
      map
        (\(exp, rep) ->
           object
             [ "config" .=
               object ["experiment" .= (exp :: String), "cores" .= cores]
             , "data" .=
               toJSON
                 (fmap
                    (\m ->
                       object
                         ["start" .= (0.0 :: Float), "finish" .= measTime m]) $
                  reportMeasured rep)
             ]) $
      zip ["sequential", "pure", "reading", "writing"] reps

reportFileName = "results/reports.json"

runExperiment lo hi fname benchmark = do
  recs <-
    forM [lo .. hi] $ \c -> do
      callProcess
        "./src/fbm_data_parallelism"
        [ "--json"
        , reportFileName
        , "--match"
        , "prefix"
        , benchmark
        , "+RTS"
        , "-N" ++ show (c :: Word)
        ]
      (_, _, recs) <- either error pure =<< readJSONReports reportFileName
      pure (c, recs)
  let avg r =
        let ms = reportMeasured r
         in sum (measTime <$> ms) / fromIntegral (V.length ms)
      outliers = ovEffect . anOutlierVar . reportAnalysis
  writeFile (fname ++ ".csv") $
    unlines $
    map
      (\(c, recs) ->
         intercalate "," $
         show c : map (show . avg) recs ++ map (show . outliers) recs)
      recs
  B.writeFile (fname ++ ".json") $ encode $ toScriptFormat recs

-- forM ["ohua-bench", "comp-bench", "app-bench", "cond-bench"] $ \benchmark -> do
main = do
  [name, cs] <- getArgs
  let (lo, hi) = read cs
  let fname = name ++ "-" ++ show lo ++ ":" ++ show hi ++ "-results"
  callProcess "stack" ["build"]
  callProcess
    "stack"
    ["ghc", "--", "src/fbm_data_parallelism.hs", "-rtsopts", "-threaded"]
  runExperiment lo hi fname name
  where
