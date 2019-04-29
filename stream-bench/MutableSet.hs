{-# language ConstraintKinds #-}
module MutableSet
    ( Set
    , Constraint
    , new
    , delete
    , insert
    , member
    , mapM_
    ) where

import Prelude hiding (mapM_)
import qualified Data.HashTable.IO as HT
import Data.Hashable (Hashable)
import Control.DeepSeq
import Data.Maybe (isJust)
import Control.Monad.IO.Class (MonadIO, liftIO)

type HashSetInner a = HT.BasicHashTable a ()
newtype Set a = Set { unwrap :: HashSetInner a }
type Constraint a = (Hashable a, Eq a)

new :: MonadIO m => m (Set a)
new = liftIO $ Set <$> HT.new

insert :: (MonadIO m, Constraint a) => a -> Set a -> m ()
insert item t = liftIO $ HT.insert (unwrap t) item ()

delete :: (MonadIO m, Constraint a) => a -> Set a -> m ()
delete item set = liftIO $ HT.delete (unwrap set) item

member :: (MonadIO m, Constraint a) => a -> Set a -> m Bool
member i t = liftIO $ isJust <$> HT.lookup (unwrap t) i

mapM_ :: MonadIO m => (a -> IO b) -> Set a -> m ()
mapM_ f = liftIO . HT.mapM_ (f . fst) . unwrap

-- This is a weird NFData instance, but the assumption is that HashMaps are
-- strict in the keys, because computing the hash forces the key. And since a
-- set does not have non-unit values, the values need not be forced.
instance NFData (Set i) where
    rnf _ = ()
