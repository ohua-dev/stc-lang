{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import BenchLib
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap as HM
import Data.List
import qualified Data.Vector as V
import System.Environment
import System.Exit
import System.IO
import System.Process as P
import Control.Exception (assert)
import System.Directory (removeFile, doesFileExist)

toScriptFormat :: [String] -> [(Word, FinishedExperiment)] -> Value
toScriptFormat variants experiments =
    toJSON
        [ object
            [ "config" .= object ["experiment" .= variant, "cores" .= cores]
            , "data" .=
              toJSON
                  (map (\(Record time) ->
                            object
                                [ "start" .= (0 :: Word)
                                , "finish" .= time
                                ])
                       (data_ result))
            ]
        | (cores, (cfg, results)) <- experiments
        , (variant, result) <-
              assert (length variants == length results) $
              zip variants results
        ]
reportFileName = "results/reports.bin"

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = doesFileExist fp >>= flip when (removeFile fp)

runExperiment :: Word -> Word -> Word -> String -> [String] -> IO ()
runExperiment lo hi iters benchmark variants = do
    let fname = benchmark ++ "-" ++ show lo ++ ":" ++ show hi ++ "-results"
    recs <-
        forM [lo .. hi] $ \c -> do
            removeFileIfExists reportFileName
            callProcess
                "stack"
                [ "run"
                , "--"
                , "bench-exec"
                ,  "-o"
                , reportFileName
                , "-i"
                , show iters
                , "-s"
                , benchmark
                , "+RTS"
                , "-N" ++ show (c :: Word)
                ]
            (c, ) <$> (decodeFile reportFileName :: IO FinishedExperiment)
  -- writeFile ("results/" ++ fname ++ ".csv") $
  --   unlines $
  --   map
  --     (\(c, recs) ->
  --        intercalate "," $
  --        show c : map (show . avg) recs ++ map (show . outliers) recs)
  --     recs
    B.writeFile ("results/" ++ fname ++ ".json") $
        encode $ toScriptFormat variants recs

-- forM ["ohua-bench", "comp-bench", "app-bench", "cond-bench"] $ \benchmark -> do

experiments :: [(String, [String])]
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

main = do
    [name, cs, iters0] <- getArgs
    let (lo, hi) = read cs
    let iters = read iters0 :: Word
    callProcess "stack" ["build"]
    let variants1
            | name == "all" = experiments
            | otherwise = [(name, getVariants name)]
    forM_ variants1 $ \(name, variants) ->
        runExperiment lo hi iters name variants
  where
    getVariants benchmark =
        case lookup benchmark experiments of
            (Just v) -> v :: [String]
            (Nothing) -> error $ "No such benchmark: " ++ benchmark
