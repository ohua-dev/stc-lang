{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Type.Magic.OldGHC
  (extractList, injectList, extractFunctor, injectFunctor
  ) where


import           Control.Exception
import           Data.Dynamic2       (Dynamic (..), TypeCastException (..))
import           Data.Maybe
import           Data.Tuple.OneTuple
import           Data.Typeable
import           Debug.Trace
import           GHC.Exts            (Any)
import           Unsafe.Coerce


extractFunctor :: forall f . (Typeable f, Functor f) => Dynamic -> f Dynamic
extractFunctor = \(Dynamic trl dl) ->
  let
    (tyCon, tyArgs) = splitTyConApp trl
  in
    if tyCon == expectedTyCon
    then
      case tyArgs of
        [tra] -> fmap f l
          where
            f = Dynamic tra
            l = unsafeCoerce dl
        _ -> error $ "Wrong kind for constructor " ++ show tyCon
    else throw $ TypeCastException expectedTy (mkTyConApp tyCon [])
  where
    !expectedTy = typeOf (Proxy :: Proxy f)
    !expectedTyCon = typeRepTyCon expectedTy


extractList :: Dynamic -> [Dynamic]
extractList = extractFunctor

injectFunctor :: forall f . (Typeable f, Functor f) => Dynamic -> f Dynamic -> Dynamic
injectFunctor (Dynamic tra _) l = Dynamic tr $ unsafeCoerce $ fmap unwrap l
  where
    unwrap (Dynamic _ v) = v
    tr = mkTyConApp (typeRepTyCon (typeOf (Proxy :: Proxy f))) [tra]


injectList [] = error "Cannot convert empty list yet"

