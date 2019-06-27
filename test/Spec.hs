{-# LANGUAGE OverloadedLists #-}
import Test.Framework

import SD.Correctness as FBM
import SD.Performance as PFBM
import Data.Typeable
import Monad.Generator
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Monad.IO.Class
import Lens.Micro (lens, Lens')
import Control.Monad.State (get, put)
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.State

import Control.Monad.Stream.Chan
import Control.Monad.Stream.Par

main :: IO ()
main =
    defaultMain
        [FBM.testSuite, PFBM.testSuite]
-- main = flip defaultMainWithOpts mempty FBM.testSuite
