{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dynamic
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Dynamic interface provides basic support for dynamic types.
--
-- Operations for injecting values of arbitrary type into
-- a dynamically typed value, Dynamic, are provided, together
-- with operations for converting dynamic values into a concrete
-- (monomorphic) type.
--
-----------------------------------------------------------------------------
module Data.Dynamic2
        -- Module Data.Typeable re-exported for convenience
    ( module Data.Typeable
        -- * The @Dynamic@ type
    , Dynamic(..) -- abstract, instance of: Show, Typeable
        -- * Converting to and from @Dynamic@
    , toDyn
    , fromDyn
    , fromDynamic
        -- * Applying functions of dynamic type
    , dynApply
    , dynApp
    , dynTypeRep
    , forceDynamic
    , TypeCastException(..)
    ) where

import Data.Maybe
import Data.Typeable
import Unsafe.Coerce

import GHC.Base
import GHC.Exception
import GHC.Show

-------------------------------------------------------------
--
--              The type Dynamic
--
-------------------------------------------------------------
{-|
  A value of type 'Dynamic' is an object encapsulated together with its type.

  A 'Dynamic' may only represent a monomorphic value; an attempt to
  create a value of type 'Dynamic' from a polymorphically-typed
  expression will result in an ambiguity error (see 'toDyn').

  'Show'ing a value of type 'Dynamic' returns a pretty-printed representation
  of the object\'s type; useful for debugging.
-}
data Dynamic =
    Dynamic TypeRep
            Obj

instance Show Dynamic
   -- the instance just prints the type representation.
                                                        where
    showsPrec _ (Dynamic t _) =
        showString "<<" . showsPrec 0 t . showString ">>"

-- here so that it isn't an orphan:
instance Exception Dynamic

type Obj = Any
 -- Use GHC's primitive 'Any' type to hold the dynamically typed value.
 --
 -- In GHC's new eval/apply execution model this type must not look
 -- like a data type.  If it did, GHC would use the constructor convention
 -- when evaluating it, and this will go wrong if the object is really a
 -- function.  Using Any forces GHC to use
 -- a fallback convention for evaluating it that works for all types.

-- | Converts an arbitrary value into an object of type 'Dynamic'.
--
-- The type of the object must be an instance of 'Typeable', which
-- ensures that only monomorphically-typed objects may be converted to
-- 'Dynamic'.  To convert a polymorphic object into 'Dynamic', give it
-- a monomorphic type signature.  For example:
--
-- >    toDyn (id :: Int -> Int)
--
toDyn :: Typeable a => a -> Dynamic
toDyn v = Dynamic (typeOf v) (unsafeCoerce v)

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDynamic'.
fromDyn ::
       Typeable a
    => Dynamic -- ^ the dynamically-typed object
    -> a -- ^ a default value
    -> a -- ^ returns: the value of the first argument, if
                        -- it has the correct type, otherwise the value of
                        -- the second argument.
fromDyn (Dynamic t v) def
    | typeOf def == t = unsafeCoerce v
    | otherwise = def

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDyn'.
fromDynamic ::
       Typeable a
    => Dynamic -- ^ the dynamically-typed object
    -> Maybe a -- ^ returns: @'Just' a@, if the dynamically-typed
                        -- object has the correct type (and @a@ is its value),
                        -- or 'Nothing' otherwise.
fromDynamic (Dynamic t v) =
    case unsafeCoerce v of
        r
            | t == typeOf r -> Just r
            | otherwise -> Nothing

-- (f::(a->b)) `dynApply` (x::a) = (f a)::b
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic t1 f) (Dynamic t2 x) =
    case funResultTy t1 t2 of
        Just t3 -> Just (Dynamic t3 ((unsafeCoerce f) x))
        Nothing -> Nothing

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f x =
    case dynApply f x of
        Just r -> r
        Nothing ->
            errorWithoutStackTrace
                ("Type error in dynamic application.\n" ++
                 "Can't apply function " ++ show f ++ " to argument " ++ show x)
#if !MIN_VERSION_base(4,9,0)
errorWithoutStackTrace = error
#endif
dynTypeRep :: Dynamic -> TypeRep
dynTypeRep (Dynamic tr _) = tr

data TypeCastException =
    TypeCastException TypeRep
                      TypeRep
    deriving (Typeable)

instance Show TypeCastException where
    show (TypeCastException expected recieved) =
        "TypeCastexception: Expected " ++
        show expected ++ " got " ++ show recieved

instance Exception TypeCastException

-- | Coerce a dynamic to a value.
-- If the expected type is not the one inside the 'Dynamic' it throws an error showing both types.
forceDynamic ::
       forall a. Typeable a
    => Dynamic
    -> a
forceDynamic dyn
    | Just a <- fromDynamic dyn = a
    | otherwise = throw $ TypeCastException rep (dynTypeRep dyn)
  where
    rep = typeRep (Proxy :: Proxy a)
