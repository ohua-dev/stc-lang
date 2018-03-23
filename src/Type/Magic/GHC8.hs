{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Type.Magic.GHC8
    ( injectList
    , extractFunctor
    , extractList
    , injectFunctor
    ) where

import Control.Exception
import Data.Dynamic2 (Dynamic(..), TypeCastException(..))
import Data.Kind
import Type.Reflection
import Unsafe.Coerce

extractFunctor ::
       forall f. (Typeable f, Functor f)
    => Dynamic
    -> f Dynamic
extractFunctor =
    \case
        Dynamic (SomeTypeRep (App con tra)) dl ->
            case con `eqTypeRep` targetRep of
                Just HRefl -> fmap f $ unsafeCoerce dl
                Nothing ->
                    throw $
                    TypeCastException (SomeTypeRep con) (SomeTypeRep targetRep)
            where f = Dynamic (SomeTypeRep tra)
        Dynamic tr _ -> throw $ TypeCastException tr (SomeTypeRep targetRep)
  where
    targetRep = typeRep @f

extractList :: Dynamic -> [Dynamic]
extractList = extractFunctor

injectFunctor ::
       forall f. (Typeable f, Functor f)
    => Dynamic
    -> f Dynamic
    -> Dynamic
injectFunctor (Dynamic (SomeTypeRep tra) _) l =
    Dynamic tr $ unsafeCoerce $ fmap unwrap l
  where
    unwrap (Dynamic _ v) = v
    tr =
        case traKind `eqTypeRep` kindStar of
            Just HRefl -> SomeTypeRep $ App (typeRep @f) tra
            Nothing ->
                throw $
                TypeCastException (SomeTypeRep traKind) (SomeTypeRep kindStar)
      where
        traKind = typeRepKind tra
        kindStar = typeRep @Type

injectList :: [Dynamic] -> Dynamic
injectList [] = error "cannot convert empty list"
injectList l@(x:_) = injectFunctor x l
