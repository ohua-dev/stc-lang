
{-# LANGUAGE OverloadedStrings #-}
import Criterion
import Criterion.IO
import Criterion.Types
import System.Process as P
import System.Environment
import qualified Data.Vector as V
import Control.Monad
import Data.List
import System.Exit
import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy as B


toScriptFormat :: [(Word, [Report])] -> Value
toScriptFormat = toJSON . concatMap mapRun
  where
    mapRun (cores, reps) =
        map (\(exp, rep) ->
                 object ["config" .= object ["experiment" .= (exp :: String), "cores" .= cores]
                        , "data" .= toJSON (fmap (\m -> object ["start" .= (0.0 :: Float), "finish" .= measTime m]) $ reportMeasured rep)
                        ]
            ) $
        zip ["sequential", "pure", "reading", "writing"] reps

reportFileName = "reports.json"

main = do
    [name, cs] <- getArgs
    let (lo, hi) = read cs
    let fname = name ++ "-" ++ show lo ++ ":" ++ show hi ++ "-results"
    callProcess "stack" ["build"]
    callProcess
        "stack"
        ["ghc", "--", "fbm_data_parallelism.hs", "-rtsopts", "-threaded"]
    recs <-
        forM [lo .. hi] $ \c -> do
            callProcess
                "./fbm_data_parallelism"
                ["--json", reportFileName, "+RTS", "-N" ++ show (c :: Word)]
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
                 show c :
                 map (show . avg) recs ++ map (show . outliers) recs)
            recs
    B.writeFile (fname ++ ".json") $ encode $ toScriptFormat recs
