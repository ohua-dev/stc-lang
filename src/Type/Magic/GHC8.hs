{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Type.Magic.GHC8 (injectList, extractFunctor, extractList) where

import           Control.Exception
import           Data.Dynamic2     (Dynamic (..), TypeCastException (..))
import           Data.Kind
import           Type.Reflection
import           Unsafe.Coerce


extractFunctor :: forall f . (Typeable f, Functor f) => Dynamic -> f Dynamic
extractFunctor = \case
  Dynamic (SomeTypeRep (App con tra)) dl ->
    case con `eqTypeRep` targetRep of
      Just HRefl -> fmap f $ unsafeCoerce dl
      Nothing -> throw $ TypeCastException (SomeTypeRep con) (SomeTypeRep targetRep)
    where
      f = Dynamic (SomeTypeRep tra)
  Dynamic tr _ -> throw $ TypeCastException tr (SomeTypeRep targetRep)
  where
    targetRep = typeRep @f

extractList :: Dynamic -> [Dynamic]
extractList = extractFunctor

injectList :: [Dynamic] -> Dynamic
injectList [] = error "Cannot convert empty list yet"
injectList l@(Dynamic (SomeTypeRep tra) _:_) = Dynamic tr $ unsafeCoerce $ map unwrap l
  where
    unwrap (Dynamic _ v) = v
    tr = case traKind `eqTypeRep` kindStar of
           Just HRefl -> SomeTypeRep $ App (typeRep @[]) tra
           Nothing    -> throw $ TypeCastException (SomeTypeRep traKind) (SomeTypeRep kindStar)
      where
        traKind = typeRepKind tra
        kindStar = typeRep @Type
