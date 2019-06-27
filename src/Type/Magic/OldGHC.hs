module Type.Magic.OldGHC
    ( extractList
    , injectList
    , extractFunctor
    , injectFunctor
    ) where

import Control.Exception
import Data.Dynamic2 (Dynamic(..), TypeCastException(..))
import Data.Typeable
import Unsafe.Coerce

extractFunctor ::
       forall f. (Typeable f, Functor f)
    => Dynamic
    -> f Dynamic
extractFunctor =
    \(Dynamic trl dl) ->
        let (tyCon, tyArgs) = splitTyConApp trl
         in if tyCon == expectedTyCon
                then case tyArgs of
                         [] ->
                             error "Constructor must be at least of kind * -> *"
                         types -> fmap f l
                             where f = Dynamic $ last types
                                   l = unsafeCoerce dl
                         _ ->
                             error $ "Wrong kind for constructor " ++ show tyCon
                else throw $ TypeCastException expectedTy (mkTyConApp tyCon [])
  where
    !expectedTy = typeRep (undefined :: Proxy f)
    !expectedTyCon = typeRepTyCon expectedTy

extractList :: Dynamic -> [Dynamic]
extractList = extractFunctor

injectFunctor ::
       forall f. (Typeable f, Functor f)
    => Dynamic
    -> f Dynamic
    -> Dynamic
injectFunctor =
    \(Dynamic tra _) l -> Dynamic (mkTy tra) $ unsafeCoerce $ fmap unwrap l
  where
    unwrap (Dynamic _ v) = v
    !targetTyRep = typeRep (undefined :: Proxy f)
    !(!con, !args) = splitTyConApp targetTyRep
    mkTy tra = mkTyConApp con (args ++ [tra])

injectList :: [Dynamic] -> Dynamic
injectList [] = error "Cannot convert empty list yet"
injectList l@(x:_) = injectFunctor x l
