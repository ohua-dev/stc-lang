{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Type.Magic.GHC8 where

import           Data.Dynamic2          (Dynamic (..))
import           Data.Kind
import           Data.Maybe
import           Data.Tuple.OneTuple
import           Data.Typeable          (cast)
import           Debug.Trace
import           Debug.Trace
import           GHC.Exts               (Any)
import           Type.Reflection
import           Type.Reflection.Unsafe
import           Unsafe.Coerce


extractList :: Dynamic -> [Dynamic]
extractList (Dynamic (SomeTypeRep (App _ tra)) dl) = map f l
  where
    f = Dynamic (SomeTypeRep tra)
    l = unsafeCoerce dl

injectList :: [Dynamic] -> Dynamic
injectList [] = error "Cannot convert empty list yet"
injectList l@(Dynamic (SomeTypeRep tra) _:_) = Dynamic tr $ unsafeCoerce $ map unwrap l
  where
    unwrap (Dynamic _ v) = v
    tr = traceShowId $ SomeTypeRep $ App (typeRep @[]) tra'
    tra' = case typeRepKind tra `eqTypeRep` typeRep @Type of
             Just HRefl -> unsafeCoerce tra
             Nothing    -> error "kinds do not match"

-- destructureTuple :: Int -> Dynamic -> Dynamic
-- destructureTuple i (Dynamic (SomeTypeRep rep) a) = Dynamic (types !! i) value
--   where
--     Con' tyCon types = rep
--     mkEntry :: forall t . Typeable t => (t -> Int -> ()) -> (TyCon, ())
--     mkEntry f = (typeRepTyCon (typeRep @t), f (unsafeCoerce a :: t) i)
--     value = unsafeCoerce $ fromMaybe (error "out of bounds") $ lookup tyCon
--       [ mkEntry $ \(OneTuple a) -> \case 0 -> a; _ -> error "out of bounds"
--       , mkEntry $ \(a, b) -> \case 0 -> a; 1 -> b; _ -> error "out of bounds"
--       , mkEntry $ \(a, b, c) -> \case 0 -> a; 1 -> b; 3 -> c; _ -> error "out of bounds"
--       , mkEntry $ \(a, b, c, d) -> \case 0 -> a; 1 -> b; 3 -> c; 4 -> d; _ -> error "out of bounds"
--       , mkEntry $ \(a, b, c, d, e) -> \case 0 -> a; 1 -> b; 3 -> c; 4 -> d; 5 -> e; _ -> error "out of bounds"
--       ]

-- lToTup :: [Dynamic] -> Dynamic
-- lToTup [] = error "cannot convert empty list"
-- lToTup l = Dynamic (SomeTypeRep $ mkTrCon tycon types) d
--   where
--     (types, items) = unzip $ map (\(Dynamic ty d) -> (ty, d)) l
--     mkEntry (a :: t) = (con, unsafeCoerce a)
--       where Con' con _ = typeRep @t
--     (tycon, d) = case map unsafeCoerce items of
--           [a] -> mkEntry $ OneTuple (a :: ())
--           [a, b] -> mkEntry (a, b)
--           [a, b, c] -> mkEntry (a, b, c)
--           [a, b, c, d] -> mkEntry (a, b, c, d)
--           _ -> error "wrapping not supported for this many arguments yet"

