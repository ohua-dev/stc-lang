module Control.Monad.SD.Case
    ( case_
    , if_
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Par.Class as PC
import Control.Monad.SD.Ohua
import Data.StateElement

import Data.List as List
import Data.Maybe

case_ ::
       forall a p. (NFData a, Show a, Eq p)
    => p
    -> [(p, OhuaM a)]
    -> OhuaM a
case_ cond patternsAndBranches = OhuaM moveState comp
  where
    moveState ::
           forall ivar m. (ParIVar ivar m, MonadIO m)
        => GlobalState ivar
        -> m (GlobalState ivar)
    moveState gs =
        (foldM (flip moveStateForward) gs . map snd) patternsAndBranches
    comp ::
           forall ivar m. (ParIVar ivar m, Monad m, MonadIO m, NFData (ivar S))
        => GlobalState ivar
        -> m (a, GlobalState ivar)
    comp gs
      -- find the first pattern that matches
     = do
        let idx = List.findIndex ((cond ==) . fst) patternsAndBranches
        let ith = fromMaybe (error "No pattern found for condition.") idx
      -- one could of course do the following in parallel but it is not a performance bottleneck as of now.
        let trueBranch = patternsAndBranches !! ith
        let falseBranches =
                ((\(before, _:after) -> before ++ after) . List.splitAt ith)
                    patternsAndBranches
        gs' <- foldM (flip moveStateForward) gs $ map snd falseBranches
        (result, gs'') <- runOhua (snd trueBranch) gs'
        return (result, gs'')

if_ :: (Show a, NFData a) => OhuaM Bool -> OhuaM a -> OhuaM a -> OhuaM a
if_ cond then_ else_ = do
    i <- cond
    case_ i [(True, then_), (False, else_)]
