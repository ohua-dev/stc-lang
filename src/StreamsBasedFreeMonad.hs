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
{-# LANGUAGE TupleSections              #-}
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
import           Control.Exception
import           Control.Monad.Free
import           Control.Parallel         (pseq)
import           Data.Dynamic
import           Data.Either
import           Data.Function
import           Data.IORef
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
    | forall inputType returnType .
        (Typeable inputType, Typeable returnType)
        =>
        Smap
            (Var inputType -> ASTM globalState (Var returnType))
            (Var [inputType])
            (Var [returnType] -> a)

data TaggedInt a = TaggedInt Integer deriving (Show, Eq, Ord)

instance Num (TaggedInt a) where
    fromInteger = TaggedInt

instance Functor (ASTAction globalState) where
    fmap f (InvokeSf sf tag vars ac) = InvokeSf sf tag vars (f . ac)
    fmap f (Smap f1 v cont)          = Smap f1 v (f . cont)
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
data NodeType globalState
    = CallSf (SFRef globalState)
    | forall inputType . Typeable inputType => SmapBegin (Proxy inputType)
    | forall outputType . Typeable outputType => Collect (Proxy outputType)
    | forall inputType . Typeable inputType => Size (Proxy inputType)
data Node globalState = Node
    { nodeType  :: NodeType globalState
    , inputRefs :: [Unique]
    , outputRef :: Unique
    }
data Algorithm globalState a = Algorithm [Node globalState] Unique
newtype Unique = Unique { uniqueToInt :: Int } deriving (Show, Eq, Ord)

instance Enum Unique where
    fromEnum = uniqueToInt
    toEnum = Unique

data Accessor s a = Accessor { getter :: s -> a, setter :: s -> a -> s }

newtype SFMonad sfState ret = SFMonad { runSFMonad :: StateT sfState IO ret }
    deriving (Typeable, Monad, Applicative, Functor, MonadIO)

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

smap :: (Typeable a, Typeable b) => (Var a -> ASTM globalState (Var b)) -> Var [a] -> ASTM globalState (Var [b])
smap f lref = liftF $ Smap f lref id

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
data B = B deriving Show
data C = C

stag :: Accessor s ()
stag = Accessor (const ()) const

sf1 :: ASTM s (Var T)
sf1 = call (liftSf (liftIO (putStrLn "Executing sf1") >> pure T :: SFMonad () T)) stag
sf2 :: Var T -> ASTM s (Var [Int])
sf2 = call (liftSf (\T -> liftIO (putStrLn "Executing sf2") >> return [0..20] :: SFMonad () [Int])) stag
sf3 :: Var T -> Var Int -> ASTM s (Var Int)
sf3 = call (liftSf (\T i -> liftIO (putStrLn "Executing sf3") >> return (succ i) :: SFMonad () Int)) stag
createAlgo :: ASTM s (Var a) -> Algorithm s a
createAlgo (ASTM m) = Algorithm w retVar
  where
    (Var retVar, _, w) = runRWS (iterM go m) () (Unique 0)
    go :: ASTAction s (RWS () [Node s] Unique b) -> RWS () [Node s] Unique b
    go (InvokeSf sf tag vars cont) =
        continue cont $ Node (CallSf $ SFRef sf tag) (map unwrapVar vars)
    go (Smap (innerCont :: Var inputType -> ASTM globalState (Var returnType)) (Var inputVar) cont) = do
        innerContVar <- freshUnique
        retVar <- freshUnique
        sizeRet <- freshUnique
        let iproxy = Proxy :: Proxy inputType
        tell
            [ Node (SmapBegin iproxy) [inputVar] retVar
            , Node (Size iproxy) [inputVar] sizeRet
            ]
        let ASTM inner = innerCont (Var innerContVar)
        v <- iterM go inner
        collReturn <- freshUnique
        -- tell $ pure $ Node Collect [unwrapVar v] collReturn
        -- cont (Var collReturn)
        continue cont (Node (Collect (Proxy :: Proxy returnType)) [sizeRet, unwrapVar v])

    continue :: (Var t -> RWS () [Node s] Unique b)
             -> (Unique -> Node s)
             -> RWS () [Node s] Unique b
    continue cont f = do
        u <- freshUnique
        tell $ pure $ f u
        cont (Var u)


class ApplyVars t where
    applyVars :: t -> [Dynamic] -> ReturnType t


instance (ApplyVars b, Typeable a) => ApplyVars (a -> b) where
    applyVars func (x:xs) = applyVars (func $ forceDynamic x) xs
    applyVars _ []        = error "too few arguments"


instance ApplyVars (SFMonad state retType) where
    applyVars res [] = res
    applyVars _ _    = error "Too many arguments"



forceDynamic :: forall a . Typeable a => Dynamic -> a
forceDynamic dyn =
    case fromDynamic dyn of
        Just a -> a
        Nothing -> error $ "Mismatching types. Expected " ++ show rep ++ " got " ++ show (dynTypeRep dyn)
  where rep = typeRep (Proxy :: Proxy a)


runAlgo :: Typeable a => Algorithm globalState a -> globalState -> IO a
runAlgo (Algorithm nodes retVar) st = do
    stateVar <- newIORef st
    let outputMap = Map.fromList $ zip (map outputRef nodes) (repeat []) :: Map.Map Unique [Chan (Either () Dynamic)]
        f (m, l) (Node func inputs oUnique) = do
            inChans <- mapM (const newChan) inputs
            let newMap = foldl (\m (u, chan) -> Map.update (Just . (chan:)) u m) m (zip inputs inChans)
            pure (newMap, (func, inChans, oUnique):l)
    (outMap, funcs) <- foldM f (outputMap, []) nodes

    retChan <- newChan

    let finalMap = Map.update (Just . (retChan:)) retVar outMap

    bracket
        (mapM (async . \(nt, inChans, retUnique :: Unique) ->
            let outChans = fromMaybe (error "return value not found") $ Map.lookup retUnique finalMap
                sendResult val = mapM_ (`writeChan` val) outChans
                sendFinish = sendResult (Left ())
                withValueFrom chan cont = readChan chan >>= either (const sendFinish) cont

                -- FIXME only send finish after list is done
                withNValuesFrom n0 chan cont = loop n0 []
                  where
                    loop 0 l = cont (reverse l)
                    loop n l
                        | n < 0 = error "invariant broken"
                        | otherwise = withValueFrom chan (loop (pred n) . (:l))
            in
            case nt of
                CallSf (SFRef sf accessor) -> runFunc sf accessor stateVar pullInputs sendResult
                  where
                    func = runFunc sf accessor stateVar
                    pullInputs
                        | null inChans = pure []
                        | otherwise = mapM readChan inChans
                SmapBegin (Proxy :: Proxy inputType)
                    | [x] <- inChans ->
                        let loop = withValueFrom x $ \y -> do
                                let ylist = forceDynamic y :: [inputType]
                                    dynYList = map toDyn ylist
                                mapM_ (sendResult . Right) dynYList
                                loop
                        in loop
                    | otherwise -> error $ "Wrong number of input arguments to smap (" ++ show (length inChans) ++ ")"
                Collect (Proxy :: Proxy returnType)
                    | [sizeChan, x] <- inChans ->
                        let loop = withValueFrom sizeChan $ \size ->
                                   withNValuesFrom (forceDynamic size :: Int) x $ \y -> do
                                        let ylist = map forceDynamic y :: [returnType]
                                        sendResult $ Right $ toDyn ylist
                                        loop
                        in loop
                    | otherwise -> error $ "Wrong number of input arguments to collect (" ++ show (length inChans) ++ ")"
                Size (Proxy :: Proxy inputType)
                    | [x] <- inChans ->
                        let loop = withValueFrom x $ sendResult . Right . dynApp (toDyn (length :: [inputType] -> Int)) >=> const loop
                        in loop

                    | otherwise -> error $ "Wrong number of input arguments to size (" ++ show (length inChans) ++ ")"
            ) funcs)
        (mapM_ cancel)
        $ \threads -> do
            mapM_ link threads
            Right ret <- readChan retChan
            Left () <- readChan retChan
            mapM_ cancel threads
            pure $ forceDynamic ret




atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref ((, ()) . f)


runFunc :: forall state inputTypes retType globalState
        .  Sf state inputTypes retType
        -> (Accessor globalState state)
        -> IORef globalState
        -> IO [Either () Dynamic]
        -> (Either () Dynamic -> IO ())
        -> IO ()
runFunc (Sf (f :: f)) accessor stTrack pullInputs sendResult = loop
  where
    runFuncOnce inVars = do
        s <- readIORef stTrack
        (ret, newState) <- runStateT (runSFMonad (applyVars f inVars)) (getter accessor s)
        atomicModifyIORef'_ stTrack (\s -> setter accessor s newState)
        pure ret
    sendFinish = sendResult (Left ())
    sendValue = sendResult . Right . toDyn
    loop = do
            allIn <- pullInputs
            case partitionEithers allIn of
                ([], []) -> do
                    runFuncOnce [] >>= sendValue
                    sendFinish
                ([], inVars) -> do
                    runFuncOnce inVars >>= sendValue
                    loop
                (_, []) -> sendFinish
                _ -> error "Some but not all inputs were closed"


algorithm :: Algorithm () [Int]
algorithm = createAlgo $ do
    var1 <- sf1
    var3 <- sf2 var1
    v <- smap (sf3 var1) var3
    pure v

freshUnique :: MonadState Unique m => m Unique
freshUnique = do
    modify succ
    S.get

data SFRef globalState
    = forall state inputTypes retType.
    SFRef (Sf state inputTypes retType) (Accessor globalState state)

printGraph (Algorithm gr ret) = do
    forM_ gr $ \(Node nt vars sfRet) ->
        let ntStr = case nt of
                        CallSf _    -> "Sf"
                        SmapBegin _ -> "Smap"
                        Collect _   -> "Collect"
                        Size _      -> "Size"
        in
        putStrLn $ ntStr ++ " with inputs " ++ show (map uniqueToInt vars) ++ " returns " ++ show (uniqueToInt sfRet)
    putStrLn $ "last return is " ++ show (uniqueToInt ret)
