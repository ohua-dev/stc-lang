{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Monad.Sbfm where

import Control.Monad
import Control.Monad.Except

-- import           Control.Monad.Par        as P
import Control.Arrow ((&&&), first)
import Control.Monad.RWS as RWS
import Control.Monad.Reader
import Control.Monad.State as S

--
-- for debugging only:
-- import Scheduler as P
-- import Control.DeepSeq
--
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Free

-- import           Control.Parallel         (pseq)
import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Loops
import Data.Default.Class
import Data.Dynamic2
import Data.Either (lefts)
import Data.Foldable (fold)
import Data.IORef
import qualified Data.IntMap as IMap
import Data.List (find, nub, sortOn)
import qualified Data.Map as Map
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void
import GHC.Exts (fromList)
import Lens.Micro
import Lens.Micro.Mtl
import Monad.Generator
import qualified Ohua.ALang.Lang as L
import qualified Ohua.ALang.Refs as ARefs
import Ohua.Compile
import qualified Ohua.Constants.HostExpr as HEConst
import qualified Ohua.DFGraph as G
import Ohua.DFLang.Lang (DFFnRef(..))
import qualified Ohua.DFLang.Refs as DFRefs
import Ohua.Monad
import Ohua.ParseTools.Refs (ohuaLangNS)
import Ohua.Types
import Ohua.Unit
import Ohua.Util
import qualified Ohua.Util.Str as Str
import System.IO (hPutStrLn, stderr)
import Type.Magic

import Control.Monad.Stream (MonadStream, Reciever, Sender)
import qualified Control.Monad.Stream as ST
import Control.Monad.Stream.Chan


type StateThread s b = StateT s IO b

-- | A type tagged tracker for where data flows inside the program
newtype Var t = Var Int

-- | The free functor, i.e., the language definition
data Lang a =
    forall sfType returnType state.
    (ReturnType sfType ~ StateThread state returnType)
    => RunST sfType Int
             (Var Void)
             (Var returnType -> a) -- continuation of the free functor
  | forall inputType returnType.
    Smap (Var inputType -> STLang (Var returnType))
            (Var [inputType])
            (Var [returnType] -> a)
  | forall returnType.
   If (Var Bool)
         (STLang (Var returnType))
         (STLang (Var returnType))
         (Var returnType -> a)

deriving instance Functor Lang

-- | The free monad
newtype STLang b = STLang (Free Lang b)
  deriving (MonadFree Lang, Monad, Applicative, Functor)

type family ReturnType (t :: *) :: * where
    ReturnType (a -> b) = ReturnType b
    ReturnType b = b

-- instance Functor Lang where
--     fmap f (RunST sf tag vars ac) = RunST sf tag vars (f . ac)
--     fmap f (Smap f1 v cont) = Smap f1 v (f . cont)
--     fmap f (If v th el cont) = If v th el (f . cont)

class STLangAPI where
  liftWithIndex :: Int -> (a -> StateThread s b) -> a -> STLang b
  runSTLang :: STLang b -> [S] -> b
  smap :: (a -> STLang b) -> [a] -> STLang [b]
  if_ :: Bool -> STLang b -> STLang b -> STLang b
