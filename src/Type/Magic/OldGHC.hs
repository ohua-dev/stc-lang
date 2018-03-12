{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Type.Magic.OldGHC
  (extractList, injectList, destructureTuple, lToTup
  ) where


import           Data.Dynamic2       (Dynamic (..))
import           Data.Maybe
import           Data.Tuple.OneTuple
import           Data.Typeable
import           Debug.Trace
import           GHC.Exts            (Any)
import           Prelude             hiding ((!!))
import qualified Prelude             as P
import           Unsafe.Coerce


(!!) :: [a] -> Int -> a
l !! i | i < 0 = error $ "Index lower than 0 (" ++ show i ++ ")"
       | i >= len = error $ "Index too large " ++ show i ++ " >= " ++ show len
       | otherwise = l P.!! i
  where len = length l

infixl 9 !!

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
    mkEntry :: forall t . Typeable t => (() -> t -> Int -> ()) -> (TyCon, ())
    mkEntry f = (tyCon, f (mkErr i tyCon) (unsafeCoerce a :: t) i)
      where tyCon = typeRepTyCon $ typeRep (Proxy :: Proxy t)

    tyConNotFound = "Could not find tycon " ++ show tyCon

    value = unsafeCoerce $ fromMaybe (error tyConNotFound) $ lookup tyCon
      [ mkEntry $ \err (OneTuple a) -> \case 0 -> a; _ -> err
      , mkEntry $ \err (a, b) -> \case 0 -> a; 1 -> b; _ -> err
      , mkEntry $ \err (a, b, c) -> \case 0 -> a; 1 -> b; 2 -> c; _ -> err
      , mkEntry $ \err (a, b, c, d) -> \case 0 -> a; 1 -> b; 2 -> c; 3 -> d; _ -> err
      , mkEntry $ \err (a, b, c, d, e) -> \case 0 -> a; 1 -> b; 2 -> c; 3 -> d; 4 -> e; _ -> err
      ]

    mkErr n ty = error $ "Index " ++ show n ++ " is out of bounds for " ++ show ty


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
