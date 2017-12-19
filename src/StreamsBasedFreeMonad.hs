{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
-- {-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}


module StreamsBasedFreeMonad where


import Control.Monad
import Control.Monad.State as S
import Control.Monad.Par as P
--
-- for debugging only:
-- import Scheduler as P
-- import Control.DeepSeq
--
import GHC.Generics (Generic)
import Debug.Trace
import Control.Parallel (pseq)
import Data.List
import Data.Function
import Control.Monad.Free

type SFM s b = State s b
type SF s a b = a -> SFM s b
instance Show (SF s a b) where
  show _ = "SF"

-- a note on the free monad:
-- the free monad is actions based! that is, the following code
--  do
--    a <- action1
--    b <- action2 a
-- is essentially the very same as just saying
--  do
--    a <- action1
--    b <- action2
-- this derives straight from the fact how >>= works in the free monad.
-- instance (Functor f) => Monad (Free f) where
--    return = Pure
--    (Free x) >>= f = Free (fmap (>>= f) x)
--    (Pure r) >>= f = f r
-- >>= is simply a recursion that pushes f into comp (the monadic computation on the left side of >>=).
-- hence, it does not really concern which of the input variables it uses!
-- (this is also true in any other monad, it is just the question whether 'action2' really needs 'a' or not.
--  this is the whole topic on desugaring do-notation! lazyness does the trick normally.)
--
-- in fact, no monad can express when a result of an action is really needed next!!!
--
-- so here is the difference between
-- data OhuaAST a b s next = SfApp a (LiftedSf s a b) (b -> next) deriving (Functor)
-- and
-- data OhuaAST a b s next = SfApp a (LiftedSf s a b) next deriving (Show,Functor)
-- in the first case, we really enforce an order of evaluation because we
-- create a function inside that needs the result of the current free-node to proceed to the next one.
-- (fmap is applied to the body of this function!)
-- in the second case, we really do not enforce this. in fact any node in the resulting
-- expression tree may execute on arbitrary input. only the semantics of the tree
-- can define the relationship between the nodes in the tree.
-- this is essentially a fundamental difference between our streams approach and the
-- composition (>>=) in a monad because the result of left-hand side may be use in the
-- computation at the right-hand side *but* the computation on the right-hand side is
-- essentially the rest of the whole computation. in the dataflow/streams approach
-- we assume that we explicitly know the data dependencies! that seems to be the very
-- reason why streams can not be expressed in terms of a monad!


-- data OhuaAST a b s next = Value a |
--                           SfApp next next |
--                           LiftedSf String Int Int (SF s a b) deriving (Show)
--
-- instance Functor (OhuaAST a b s) where
--   fmap f (Value v) = Value v -- 1. functor law
--   fmap f (LiftedSf name fnIdx ident sf) = LiftedSf name fnIdx ident sf -- 1. functor law
--   fmap f (SfApp l r) = SfApp (f l) (f r) -- 2. functor law

data LiftedSf s a b = LiftedSf String Int Int (SF s a b) deriving (Show)

--
-- In this version of the Free Monad, the succeeding (next) computation is
-- encapsulated as a lambda function. That is, nothing was executed yet in the
-- received expression tree.
--
-- data OhuaAST a b s next = SfApp a (LiftedSf s a b) (b -> next) deriving (Functor)
-- data OhuaAST a b s next = SfApp a (LiftedSf s a b) next deriving (Show,Functor)

-- data IList a = Cons a (IVar (IList a)) | Last a deriving (Generic)
-- newtype SList a = SList (IVar (IList a))
-- instance NFData a => NFData (IList a)
--
-- execSF :: a -> LiftedSf s a b -> s -> (b,s)
-- execSF input (LiftedSf name fnIdx ident sf) gs = runState (sf a) gs
--
-- execDFKernel :: LiftedSf s a b -> SList a -> SList b -> s -> Par s
-- execDFKernel sf inS outS gs = do
--   i <- trace "exec DF kernel'" $ P.get inS
--   case i of
--     (Cons x xs) -> do
--       (r,gs') <- execSF x sf gs
--       tl <- trace ("execKernel: " ++ (show (length l))) new
--       r `pseq` P.put_ outS $ Cons r tl
--       execDFKernel sf xs tl gs'
--     (Last x) -> do
--       (r,gs') <- trace "executing sf" $ execSF x sf gs
--       r `pseq` P.put_ outS $ Last r
--       return gs'

-- instance (Show a) => Show (OhuaAST a b s next) where
--   show (SfApp input lsf n) =
--     let r runState lsf
--     "(SfApp " ++ show input ++ " " ++ show lsf ++ " " ++ show (n ()) ++ ")"

-- instance Functor (OhuaAST a b s) where
--   fmap f (Value v) = Value v -- 1. functor law
--   fmap f (LiftedSf name fnIdx ident sf) = LiftedSf name fnIdx ident sf -- 1. functor law
--   fmap f (SfApp l r) = SfApp (f l) (f r) -- 2. functor law


-- data OhuaAST a b s next = SfApp a (LiftedSf s a b) (b -> next) deriving (Functor)
-- (Ohua a b s) refers to the functor of "next"
-- liftWithIndex :: (Show a) => String -> Int -> SF s a b -> Int -> a -> Free (OhuaAST a b s) b
-- liftWithIndex name fnIdx sf ident input =
--   trace (name ++ ": " ++ show input) $ liftF (SfApp input (LiftedSf name fnIdx ident sf) id)

data OhuaAST a b s next = SfApp a (LiftedSf s a b) next deriving (Show,Functor)

-- liftWithIndex :: (Show a) => String -> Int -> SF s a b -> Int -> a -> Free (OhuaAST a b s) a
-- liftWithIndex name fnIdx sf ident input =
--   trace (name ++ ": " ++ show input) $ liftF (SfApp input (LiftedSf name fnIdx ident sf) input)

liftWithIndex :: (Show a) => String -> Int -> SF s a b -> Int -> a -> Free (OhuaAST a b s) ()
liftWithIndex name fnIdx sf ident input =
  trace (name ++ ": " ++ show input) $ liftF (SfApp input (LiftedSf name fnIdx ident sf) ())


runOhuaM :: (Show a, Show b, Show s) => Free (OhuaAST a b s) () -> [s] -> (a,[s])
runOhuaM comp state = prettyPrinter comp state

prettyPrinter :: (Show a, Show b, Show s) => Free (OhuaAST a b s) () -> [s] -> (a,[s])
prettyPrinter ast state = (printAST ast,state)
  where
    printAST ast = trace ("\nAST: " ++ show ast) undefined

-- interpret :: Free (OhuaM a b s) b -> [s] -> Par (b,[s])
-- interpret ast@(Free (Sfc l r)) initialState = do
