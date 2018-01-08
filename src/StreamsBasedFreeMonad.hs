{-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module StreamsBasedFreeMonad where


import           Control.Monad
import           Control.Monad.Par        as P
import           Control.Monad.RWS        as RWS
import           Control.Monad.State      as S
import           Control.Monad.Writer     as W
--
-- for debugging only:
-- import Scheduler as P
-- import Control.DeepSeq
--
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad.Free
import           Control.Parallel         (pseq)
import           Data.Dynamic
import           Data.Either
import           Data.Function
import           Data.Kind
import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Typeable
import           Data.Void
import           Debug.Trace
import           GHC.Generics             (Generic)

-- type SFM s b = State s b
-- type SF s a b = a -> SFM s b
-- instance Show (SF s a b) where
--   show _ = "SF"

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
--
-- in order to really find these direct dependencies, our API would have to somehow
-- directly integrate the concept of variables. we would have to leave the world of
-- "standard haskell" and would in the end start to define our very own language, e.g.,
-- data Ohua = Var String | SFC (SF s a b) idx | Let
-- programs would then look something like:
-- do
--  Let (Var "a") (sfc foo 0)
--      (SFC bar 1 (Var a))
-- it is basically what we already have implemented in the "real" Ohua compiler!


-- data OhuaAST a b s next = Value a |
--                           SfApp next next |
--                           LiftedSf String Int Int (SF s a b) deriving (Show)
--
-- instance Functor (OhuaAST a b s) where
--   fmap f (Value v) = Value v -- 1. functor law
--   fmap f (LiftedSf name fnIdx ident sf) = LiftedSf name fnIdx ident sf -- 1. functor law
--   fmap f (SfApp l r) = SfApp (f l) (f r) -- 2. functor law

-- data LiftedSf s a b = LiftedSf String Int Int (SF s a b) deriving (Show)

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

-- data OhuaAST a b s next = SfApp a (LiftedSf s a b) next deriving (Show,Functor)

-- liftWithIndex :: (Show a) => String -> Int -> SF s a b -> Int -> a -> Free (OhuaAST a b s) a
-- liftWithIndex name fnIdx sf ident input =
--   trace (name ++ ": " ++ show input) $ liftF (SfApp input (LiftedSf name fnIdx ident sf) input)

-- liftWithIndex :: (Show a) => String -> Int -> SF s a b -> Int -> a -> Free (OhuaAST a b s) ()
-- liftWithIndex name fnIdx sf ident input =
--   trace (name ++ ": " ++ show input) $ liftF (SfApp input (LiftedSf name fnIdx ident sf) ())


-- runOhuaM :: (Show a, Show b, Show s) => Free (OhuaAST a b s) () -> [s] -> (a,[s])
-- runOhuaM comp state = prettyPrinter comp state

-- prettyPrinter :: (Show a, Show b, Show s) => Free (OhuaAST a b s) () -> [s] -> (a,[s])
-- prettyPrinter ast state = (printAST ast,state)
--   where
--     printAST ast = trace ("\nAST: " ++ show ast) undefined

-- interpret :: Free (OhuaM a b s) b -> [s] -> Par (b,[s])
-- interpret ast@(Free (Sfc l r)) initialState = do

newtype ASTM globalState a = ASTM (Free (ASTAction globalState) a)
    deriving (MonadFree (ASTAction globalState), Monad, Applicative, Functor)
data ASTAction globalState a
    = forall inputTypes sfState returnType . InvokeSf
        (Sf sfState inputTypes returnType)
        (Accessor globalState sfState)
        [Var Void] -- reference to the input vars
        (Var returnType -> a) -- continutation

data TaggedInt a = TaggedInt Integer deriving (Show, Eq, Ord)

instance Num (TaggedInt a) where
    fromInteger = TaggedInt

instance Functor (ASTAction globalState) where
    fmap f (InvokeSf sf tag vars ac) = InvokeSf sf tag vars (f . ac)
newtype Var t = Var { unwrapVar :: Unique }
-- make state be input
data Sf state (inputTypes :: [Type]) returnType
    = forall fnType .
    ( fnType ~ ToFunc inputTypes (SFMonad state returnType)
    , inputTypes ~ InputList fnType
    , ReturnType fnType ~ SFMonad state returnType
    , Typeable returnType
    , ApplyVars fnType
    ) => Sf fnType -- reference to the actual function
data Algorithm globalState a = Algorithm [(SFRef globalState, [Unique], Unique)] Unique
newtype Unique = Unique { uniqueToInt :: Int } deriving (Show, Eq, Ord)

instance Enum Unique where
    fromEnum = uniqueToInt
    toEnum = Unique

data Accessor s a = Accessor { getter :: s -> a, setter :: s -> a -> s }

newtype SFMonad sfState ret = SFMonad { runSFMonad :: StateT sfState IO ret }
    deriving (Typeable, Monad, Applicative, Functor)

-- | This type level function collects a list of input arguments for a given function
-- So long as the input type is a function type, aka (->) it recurses onto the rhs.
--
-- aka
-- @
--  InputList b             = []
--  InputList (a -> b)      = [a]
--  InputList (a -> b -> c) = [a, b]
-- @
--
-- Together with 'ReturnType' this can be used to decompose function types
type family InputList (t :: Type) :: [Type] where
    InputList (a -> b) = a : InputList b
    InputList a = '[]

-- | This type level function retrieves the return type of a function with any arity.
-- So long as the input type is a function type, aka (->) it recurses onto the rhs.
--
-- aka
-- @
--  ReturnType b             = b
--  ReturnType (a -> b)      = b
--  ReturnType (a -> b -> c) = c
-- @
--
-- Together with 'InputList' this can be used to decompose function types
type family ReturnType (t :: Type) :: Type where
    ReturnType (a -> b) = ReturnType b
    ReturnType b = b

liftSf ::
    ( ReturnType f ~ SFMonad state ret
    , Typeable ret
    , ToFunc (InputList f) (SFMonad state ret) ~ f
    , ApplyVars f
    )
    => f -> Sf state (InputList f) ret
liftSf f = Sf f


-- | Construct a function type from a list of input parameters and a final return type
--
-- @
--  ToFunc [] a     = a
--  ToFunc [a] b    = a -> b
--  ToFunc [a, b] c = a -> b -> c
-- @
--
-- This is the inverse operation of the function destructuring from 'InputList' and 'ReturnType'
type family ToFunc (ts :: [Type]) (ret :: Type) :: Type where
    ToFunc (x:xs) ret = x -> ToFunc xs ret
    ToFunc '[] ret = ret

-- | A type level 'map' over a list of types
--
-- @
--  MapTypeList f [a, b, c] = [f a, f b, f c]
-- @
type family MapTypeList (f :: Type -> Type) (ts :: [Type]) :: [Type] where
    MapTypeList f (t:ts) = f t : MapTypeList f ts
    MapTypeList _ '[] = '[]


call :: ( asFnType ~ ToFunc (MapTypeList Var ts) (ASTM globalState (Var ret))
        , Ret asFnType ~ ret
        , GlobalState asFnType ~ globalState
        , CollectVars asFnType
        ) => Sf state ts ret -> Accessor globalState state -> asFnType
call sf accessor = collectVars [] $ \vars -> liftF $ InvokeSf sf accessor (reverse vars) id

toVoidVar :: Var a -> Var Void
toVoidVar (Var u) = Var u

-- | Helper class to make 'call' a multi arity function
class CollectVars t where
    type Ret t
    type GlobalState t
    collectVars :: [Var Void] -> ([Var Void] -> ASTM (GlobalState t) (Var (Ret t))) -> t

instance (Typeable a, CollectVars b) => CollectVars (Var a -> b) where
    type Ret (Var a -> b) = Ret b
    type GlobalState (Var a -> b) = GlobalState b
    collectVars l f v = collectVars (toVoidVar v : l) f

instance CollectVars (ASTM gs (Var a)) where
    type Ret (ASTM gs (Var a)) = a
    type GlobalState (ASTM gs (Var a)) = gs
    collectVars l f = f l

data T = T
data A = A
data B = B
data C = C

stag :: Accessor s ()
stag = Accessor (const ()) const

sf1 :: ASTM s (Var T)
sf1 = call (liftSf (return T :: SFMonad () T)) stag
sf2 :: Var T -> ASTM s (Var A)
sf2 = call (liftSf (\T -> return A :: SFMonad () A)) stag
sf3 :: Var T -> Var A -> ASTM s (Var B)
sf3 = call (liftSf (\T A -> return B :: SFMonad () B)) stag
createAlgo :: ASTM s (Var a) -> Algorithm s a
createAlgo (ASTM m) = Algorithm w retVar
  where
    (Var retVar, _, w) = runRWS (iterM f m) () (Unique 0)
    f (InvokeSf sf tag vars cont) = do
        retRef <- freshUnique
        tell $ pure
            ( SFRef sf tag
            , map unwrapVar vars
            , retRef
            )
        cont $ Var retRef


class ApplyVars t where
    applyVars :: t -> [Dynamic] -> ReturnType t


instance (ApplyVars b, Typeable a) => ApplyVars (a -> b) where
    applyVars func (x:xs) = applyVars (maybe (error "Coercion failed") func $ fromDynamic x) xs
    applyVars _ [] = error "too few arguments"


instance ApplyVars (SFMonad state retType) where
    applyVars res [] = res
    applyVars _ _    = error "Too many arguments"


runAlgo :: Typeable a => Algorithm globalState a -> globalState -> IO a
runAlgo (Algorithm nodes retVar) st = do
    stateVar <- newMVar st
    let outputMap = Map.fromList $ zip (map (\(_, _, o) -> o) nodes) (repeat []) :: Map.Map Unique [Chan (Either () Dynamic)]
        f (m, l) (func, inputs, oUnique) = do
            inChans <- mapM (const newChan) inputs
            let newMap = foldl (\m (u, chan) -> Map.update (Just . (chan:)) u m) m (zip inputs inChans)
            pure (newMap, (func, inChans, oUnique):l)
    (outMap, funcs) <- foldM f (outputMap, []) nodes

    retChan <- newChan

    let finalMap = Map.update (Just . (retChan:)) retVar outMap

    mapM_ (link <=< async . \(SFRef sf tag, inChans, retUnique :: Unique) -> runFunc sf tag inChans (fromMaybe (error "return value not found") $ Map.lookup retUnique finalMap) stateVar) funcs
    Right ret <- readChan retChan
    Left () <- readChan retChan
    pure $ fromMaybe (error "wrong type") $ fromDynamic ret


runFunc :: forall state inputTypes retType globalState.
        Sf state inputTypes retType
        -> (Accessor globalState state)
        -> [Chan (Either () Dynamic)] -> [Chan (Either () Dynamic)]
        -> MVar globalState -> IO ()
runFunc (Sf (f :: f)) accessor inChans outChans stTrack = loop
  where
    runFuncOnce inVars = do
        s <- readMVar stTrack
        (ret, newState) <- runStateT (runSFMonad (applyVars f inVars)) (getter accessor s)
        putMVar stTrack (setter accessor s newState)
        pure ret
    sendResult ret = mapM_ (`writeChan` Right (toDyn (ret :: retType))) outChans
    sendFinish = mapM_ (`writeChan` Left ()) outChans
    loop | null inChans = do
            runFuncOnce [] >>= sendResult
            sendFinish
         | otherwise = do
            allIn <- mapM readChan inChans
            case partitionEithers allIn of
                ([], inVars) -> do
                    runFuncOnce inVars >>= sendResult
                    loop
                (_, []) -> sendFinish
                _ -> error "Some but not all inputs were closed"


algorithm :: Algorithm () B
algorithm = createAlgo $ do
    var1 <- sf1
    var3 <- sf2 var1
    v <- sf3 var1 var3
    pure v

freshUnique :: MonadState Unique m => m Unique
freshUnique = do
    modify succ
    S.get

data SFRef globalState
    = forall state inputTypes retType.
    SFRef (Sf state inputTypes retType) (Accessor globalState state)

printGraph (Algorithm gr ret) = do
    forM_ gr $ \(SFRef _ _, vars, sfRet) ->
        putStrLn $ "Sf with inputs " ++ show (map uniqueToInt vars) ++ " returns " ++ show (uniqueToInt sfRet)
    putStrLn $ "last return is " ++ show (uniqueToInt ret)
