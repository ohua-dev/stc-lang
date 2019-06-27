{-# LANGUAGE InstanceSigs #-}

module Data.StateElement where

import Data.Dynamic2

import Control.DeepSeq

--
-- Support for heterogeneous lists.
--
data S =
    forall a. Typeable a =>
              S (a -> ())
                Dynamic

toS :: forall a. (Typeable a, NFData a)
    => a
    -> S
toS a = S rnf' (toDyn a)
  where
    rnf' :: a -> ()
    rnf' = rnf

fromS :: Typeable a => S -> a
fromS (S _ a) = forceDynamic a

instance NFData S where
    rnf :: S -> ()
    rnf (S toRnf d) = toRnf $ forceDynamic d
