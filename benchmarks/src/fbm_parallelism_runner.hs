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
import System.Environment
import System.Exit
import System.IO
import System.Process as P

toScriptFormat :: [String] -> [(Word, [Report])] -> Value
toScriptFormat variants = toJSON . concatMap (mapRun variants)
  where
    mapRun variants (cores, reps) =
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
      zip variants reps

reportFileName = "results/reports.json"

runExperiment lo hi benchmark variants = do
  let fname = benchmark ++ "-" ++ show lo ++ ":" ++ show hi ++ "-results"
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
  writeFile ("results/" ++ fname ++ ".csv") $
    unlines $
    map
      (\(c, recs) ->
         intercalate "," $
         show c : map (show . avg) recs ++ map (show . outliers) recs)
      recs
  B.writeFile ("results/" ++ fname ++ ".json") $
    encode $ toScriptFormat variants recs

-- forM ["ohua-bench", "comp-bench", "app-bench", "cond-bench"] $ \benchmark -> do
setUpExperiment = do
  callProcess "stack" ["build"]
  callProcess
    "stack"
    ["ghc", "--", "src/fbm_data_parallelism.hs", "-rtsopts", "-threaded"]

experiments =
  [ ("ohua-bench", ["sequential", "pure", "reading", "writing"])
  , ("comp-bench", ["sequential", "ohua", "par1", "par2"])
  , ("app-bench", ["sequential", "ohua", "par"])
  , ("cond-bench", ["sequential", "ohua", "par"])
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
