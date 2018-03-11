{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Type.Magic.OldGHC where


import           Data.Dynamic2       (Dynamic (..))
import           Data.Maybe
import           Data.Tuple.OneTuple
import           Data.Typeable
import           Debug.Trace
import           GHC.Exts            (Any)
import           Unsafe.Coerce


extractList :: Dynamic -> [Dynamic]
extractList (Dynamic trl dl) = map f l
  where
    f = Dynamic tra
    [tra] = typeRepArgs trl
    l = unsafeCoerce dl

injectList :: [Dynamic] -> Dynamic
injectList [] = error "Cannot convert empty list yet"
injectList l@(Dynamic tra _:_) = Dynamic tr $ unsafeCoerce $ map unwrap l
  where
    unwrap (Dynamic _ v) = v
    tr = mkTyConApp (typeRepTyCon (typeRep (Proxy :: Proxy [()]))) [tra]

destructureTuple :: Int -> Dynamic -> Dynamic
destructureTuple i (Dynamic rep a) = Dynamic (types !! i) value
  where
    (tyCon, types) = splitTyConApp rep
    mkEntry :: forall t . Typeable t => (t -> Int -> ()) -> (TyCon, ())
    mkEntry f = (typeRepTyCon (typeRep (Proxy :: Proxy t)), f (unsafeCoerce a :: t) i)
    value = unsafeCoerce $ fromMaybe (error "out of bounds") $ lookup tyCon
      [ mkEntry $ \(OneTuple a) -> \case 0 -> a; _ -> error "out of bounds"
      , mkEntry $ \(a, b) -> \case 0 -> a; 1 -> b; _ -> error "out of bounds"
      , mkEntry $ \(a, b, c) -> \case 0 -> a; 1 -> b; 3 -> c; _ -> error "out of bounds"
      , mkEntry $ \(a, b, c, d) -> \case 0 -> a; 1 -> b; 3 -> c; 4 -> d; _ -> error "out of bounds"
      , mkEntry $ \(a, b, c, d, e) -> \case 0 -> a; 1 -> b; 3 -> c; 4 -> d; 5 -> e; _ -> error "out of bounds"
      ]


lToTup :: [Dynamic] -> Dynamic
lToTup [] = error "cannot convert empty list"
lToTup l = Dynamic (mkTyConApp tycon types) d
  where
    (types, items) = unzip $ map (\(Dynamic ty d) -> (ty, d)) l
    mkEntry (a :: t) = (typeRepTyCon (typeRep (Proxy :: Proxy t)), unsafeCoerce a)
    (tycon, d) = case map unsafeCoerce items of
          [a] -> mkEntry $ OneTuple (a :: ())
          [a, b] -> mkEntry (a, b)
          [a, b, c] -> mkEntry (a, b, c)
          [a, b, c, d] -> mkEntry (a, b, c, d)
          _ -> error "wrapping not supported for this many arguments yet"
