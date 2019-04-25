{-# language ConstraintKinds #-}
module MutableSet (Set, Constraint, new, delete, insert, member) where

import qualified Data.HashTable.IO as HT
import Data.Hashable (Hashable)
import Control.DeepSeq
import Data.Maybe (isJust)

type HashSetInner a = HT.BasicHashTable a ()
newtype Set a = Set { unwrap :: HashSetInner a }
type Constraint a = (Hashable a, Eq a)

new :: IO (Set a)
new = Set <$> HT.new

insert :: Constraint a => a -> Set a -> IO ()
insert item t = HT.insert (unwrap t) item ()

delete :: Constraint a => a -> Set a -> IO ()
delete = flip (HT.delete . unwrap)

member :: Constraint a => a -> Set a -> IO Bool
member i t = isJust <$> HT.lookup (unwrap t) i

-- This is a weird NFData instance, but the assumption is that HashMaps are
-- strict in the keys, because computing the hash forces the key. And since a
-- set does not have non-unit values, the values need not be forced.
instance NFData (Set i) where
    rnf _ = ()
