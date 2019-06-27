{-# LANGUAGE ConstraintKinds #-}

module MutableNFMap
    ( Map
    , new
    , insert
    , delete
    , lookup
    , mapM_
    , size
    ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import qualified Data.HashTable.IO as MHT
import Data.Hashable (Hashable)
import Prelude hiding (lookup, mapM_)

type Constraint a = (Hashable a, Eq a)

newtype Map k v = Map
    { unwrap :: MHT.BasicHashTable k v
    }

-- | This instance does nothing, because the functions exposed force the keys
-- and values automatically
instance NFData (Map k v) where
    rnf _ = ()

new :: MonadIO m => m (Map k v)
new = liftIO $ Map <$> MHT.new

-- | I only require an 'NFData' instance for the values, because the assumption
-- is that calculating the hash for the keys will force them.
insert :: (Constraint k, NFData v, MonadIO m) => k -> v -> Map k v -> m ()
insert k v m = liftIO $ v `deepseq` MHT.insert (unwrap m) k v

delete :: (Constraint k, MonadIO m) => k -> Map k v -> m ()
delete k m = liftIO $ MHT.delete (unwrap m) k

lookup :: (Constraint k, MonadIO m) => k -> Map k v -> m (Maybe v)
lookup k m = liftIO $ MHT.lookup (unwrap m) k

mapM_ :: MonadIO m => ((k, v) -> IO a) -> Map k v -> m ()
mapM_ f = liftIO . MHT.mapM_ f . unwrap

size :: MonadIO m => Map k v -> m Word
size = liftIO . MHT.foldM (\a _ -> pure $ a + 1) 0 . unwrap
