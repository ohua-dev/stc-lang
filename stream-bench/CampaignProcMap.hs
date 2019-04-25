{-# LANGUAGE ConstraintKinds #-}
module CampaignProcMap (Map, new, insert, mapM_) where

import Prelude hiding (mapM_)
import Data.Hashable (Hashable)
import Control.DeepSeq
import qualified MutableSet as Set
import qualified Data.HashTable.IO as MHT
import Control.Monad.IO.Class

type Constraint a = (Hashable a, Eq a)

newtype Map k v = Map
    { unwrap :: MHT.BasicHashTable k (Set.Set v)
    }

-- This may seem like an odd instance for NFData, but as with the `HashSet` type
-- my reasoning is that 'HashTable' is strict in the keys, thus we don't need to
-- force them. And since the values are 'HashSet', which is also completely
-- strict already we don't need to specially evaluate it.
instance NFData (Map k v) where
    rnf _ = ()

new :: IO (Map k v)
new = Map <$> MHT.new

insert :: (Constraint k, Set.Constraint v, MonadIO m) => k -> v -> Map k v -> m ()
insert k v m = liftIO $ do
    set <- maybe Set.new pure =<< MHT.lookup (unwrap m) k
    Set.insert v set

mapM_ :: MonadIO m => ((k, Set.Set v) -> IO a) -> Map k v -> m ()
mapM_ f = liftIO . MHT.mapM_ f . unwrap
