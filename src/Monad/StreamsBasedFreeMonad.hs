{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PolyKinds #-}


-- TODO define the API here by exposing it.
module Monad.StreamsBasedFreeMonad
    ( -- ** Data types
      Unique
    , Var
    , SfMonad(..)
    , Sf(..)
    , Accessor
    , ASTM
    , SfRef(SfRef)
    , NodeType(..)
    , Node(..)
    , Algorithm(..)
    , FunctionDict
    , UDict
    -- ** helper functions
    , liftSf
    , liftSfNamed
    , sfm
    , call
    , sfConst
    , gt
    -- ** builtins
    , smap
    , smapGen
    , if_
    , generate
    -- ** Running
    , createAlgo
    , defaultFunctionDict
    , evaluateAST
    , runCompiler
    , runAlgo
    , runAlgoWStats
    -- ** The stream monad
    , Packet
    , StreamM
    , StreamInit
    , sendPacket
    , abortProcessing
    , endProcessingAt
    , endProcessing
    , expectFinish
    , sendEOS
    , recievePacket
    , recieveUntyped
    , recieveAllUntyped
    , recieve
    , send
    , sendUntyped
    , recieveAll
    , runFunc
    , mountStreamProcessor
    , ExecutionException(..)
    , withIsAllowed
    -- ** Type level functions
    , ReturnType
    , MapFnType
    , SetReturnType
    -- ** Misc
    , algorithm
    , stag
    , T
    , makeDestructuringExplicit
    , united
    , Stats
    ) where

import Control.Monad
import Control.Monad.Except

-- import           Control.Monad.Par        as P
import Control.Arrow ((&&&), first)
import Control.Monad.RWS as RWS
import Control.Monad.Reader
import Control.Monad.State as S

--
-- for debugging only:
-- import Scheduler as P
-- import Control.DeepSeq
--
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Free

-- import           Control.Parallel         (pseq)
import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Loops
import Data.Default.Class
import Data.Dynamic2
import Data.Either (lefts)
import Data.Foldable (fold)
import Data.IORef
import qualified Data.IntMap as IMap
import Data.List (find, nub, sortOn,genericLength)
import qualified Data.Map as Map
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void
import GHC.Exts (fromList)
import Lens.Micro
import Lens.Micro.Mtl
import Monad.Generator
import qualified Ohua.ALang.Lang as L
import qualified Ohua.ALang.Refs as ARefs
import Ohua.Compile
import qualified Ohua.Constants.HostExpr as HEConst
import qualified Ohua.DFGraph as G
import Ohua.DFLang.Lang (DFFnRef(..))
import qualified Ohua.DFLang.Refs as DFRefs
import Ohua.Monad
import Ohua.ParseTools.Refs (ohuaLangNS)
import Ohua.Types
import Ohua.Unit
import Ohua.Util
import qualified Ohua.Util.Str as Str
import System.IO (hPutStrLn, stderr)
import System.CPUTime
import Type.Magic
import Text.Printf

import Control.Monad.Stream (MonadStream, Reciever, Sender)
import qualified Control.Monad.Stream as ST
import Control.Monad.Stream.Chan

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import Ohua.Serialize.JSON

united :: Lens' s ()
united f s = const s <$> f ()

logError :: MonadIO m => String -> m ()
logError = liftIO . hPutStrLn stderr

-- The free monad
-- | A unique thing
data Unique = Unique
    { uniqueToInt :: Int
    } deriving (Ord, Eq)

instance Enum Unique where
    fromEnum = uniqueToInt
    toEnum = Unique

-- | A type tagged tracker for where data flows inside the program
newtype Var t =
    Var Unique

newtype FunctorTaggedBinding f = FunctorTaggedBinding QualifiedBinding

-- | The monad for the stateful function to run in
newtype SfMonad sfState ret = SfMonad
    { runSfMonad :: StateT sfState IO ret
    } deriving ( Typeable
               , Monad
               , Applicative
               , Functor
               , MonadIO
               , MonadState sfState
               )

-- | A stateful function nicely packaged with all its required capabilities
data Sf fnType =
    forall state returnType. ( ReturnType fnType ~ SfMonad state returnType
                             , Typeable returnType
                             , ApplyVars fnType
                             ) =>
                             Sf fnType
                                (Maybe QualifiedBinding) -- reference to the actual function

-- | A way to retrieve and update local state in a larger state
-- structure
type Accessor s a = Lens' s a

-- | Things you can do in the AST Monad
data ASTAction globalState a
    = forall sfType returnType sfState. (ReturnType sfType ~ SfMonad sfState returnType) =>
                                        InvokeSf (Sf sfType)
                                                 (Accessor globalState sfState)
                                                 [Var Void] -- reference to the input vars
                                                 (Var returnType -> a) -- continuation
    | forall inputType returnType f. ( Typeable inputType
                                     , Typeable returnType
                                     , Functor f
                                     ) =>
                                     SmapLike (FunctorTaggedBinding f)
                                              (Var inputType -> ASTM globalState (Var returnType))
                                              (Var (f inputType))
                                              (Var (f returnType) -> a)
    | forall returnType. Typeable returnType =>
                         If (Var Bool)
                            (ASTM globalState (Var returnType))
                            (ASTM globalState (Var returnType))
                            (Var returnType -> a)
    | forall returnType. Typeable returnType =>
                         Generate (ASTM globalState (Var (Maybe returnType)))
                                  (Var (Generator IO Dynamic) -> a)

-- | The AST Monad
newtype ASTM globalState a =
    ASTM (Free (ASTAction globalState) a)
    deriving (MonadFree (ASTAction globalState), Monad, Applicative, Functor)

instance Functor (ASTAction globalState) where
    fmap f (InvokeSf sf tag vars ac) = InvokeSf sf tag vars (f . ac)
    fmap f (SmapLike bnd f1 v cont) = SmapLike bnd f1 v (f . cont)
    fmap f (If v th el cont) = If v th el (f . cont)
    fmap f (Generate gf cont) = Generate gf (f . cont)

-- | This type level function retrieves the return type of a function
-- with any arity.  So long as the input type is a function type, aka
-- (->) it recurses onto the rhs.
--
-- aka
-- @
--  ReturnType b             = b
--  ReturnType (a -> b)      = b
--  ReturnType (a -> b -> c) = c
-- @
--
-- Together with 'InputList' this can be used to decompose function
-- types
type family ReturnType (t :: *) :: * where
    ReturnType (a -> b) = ReturnType b
    ReturnType b = b

-- | Make a stateful function from some arbitrary function of type @f@
--
-- @f@ is not required to have any arguments but must return in the
-- 'SfMonad'
liftSf ::
       (ReturnType f ~ SfMonad state ret, Typeable ret, ApplyVars f)
    => f
    -> Sf f
liftSf f = Sf f Nothing

liftSfNamed ::
       (ReturnType f ~ SfMonad state ret, Typeable ret, ApplyVars f)
    => QualifiedBinding
    -> f
    -> Sf f
liftSfNamed name f = Sf f (Just name)

-- | A convenience function. It doesn't actually do anything (@sfm =
-- id@) but it has a concrete type and therefore can be used to help
-- the compiler when it infers the type of a function pased to
-- 'liftSf'.
--
-- Because of the type level computation used on the function type the
-- functions often have to be annotated like so @liftSf (\\... ->
-- ... :: SfMonad t0 t1)@ or otherwise the compiler is unable to infer
-- the type.  The same can be achieved with this function @liftSf
-- (\\... -> sfm $ ...)@.
sfm :: SfMonad s a -> SfMonad s a
sfm = id

-- | Map a function type by wrapping each argument in a type with kind @* -> *@
type family MapFnType (f :: * -> *) t where
    MapFnType f (a -> b) = f a -> MapFnType f b
    MapFnType _c b = b

-- | Set the return type of function @f@ to @t@
type family SetReturnType f t where
    SetReturnType f (a -> b) = a -> SetReturnType f b
    SetReturnType f _c = f

-- | "Call" a stateful function.  This takes a lifted function (see
-- 'liftSf'), links it with an accessor for the state that this
-- function uses and produces a function that can be used in 'ASTM'
call ::
       ( newFnType ~ SetReturnType (ASTM globalState (Var ret)) (MapFnType Var f)
       , ReturnType f ~ SfMonad state ret
       , Ret newFnType ~ ret
       , GlobalState newFnType ~ globalState
       , CollectVars newFnType
       )
    => Sf f
    -> Accessor globalState state
    -> newFnType
call sf accessor =
    collectVars [] $ \vars -> liftF $ InvokeSf sf accessor (reverse vars) id

-- | Goodbye typesafety :(
toVoidVar :: Var a -> Var Void
toVoidVar (Var u) = Var u

tSmap :: FunctorTaggedBinding []
tSmap = FunctorTaggedBinding ARefs.smap

tSmapG :: FunctorTaggedBinding (Generator IO)
tSmapG = FunctorTaggedBinding ARefs.smapG

-- | The smap primitive. Applies an ohua program to each value from a
-- collection.  The semantics guarantee that the values are processed
-- in-order with respects to each node in the subgraph.
smap ::
       (Typeable a, Typeable b)
    => (Var a -> ASTM globalState (Var b))
    -> Var [a]
    -> ASTM globalState (Var [b])
smap f lref = liftF $ SmapLike tSmap f lref id

smapGen ::
       (Typeable a, Typeable b)
    => (Var a -> ASTM globalState (Var b))
    -> Var (Generator IO a)
    -> ASTM globalState (Var (Generator IO b))
smapGen f lref = liftF $ SmapLike tSmapG f lref id

if_ :: Typeable a
    => Var Bool
    -> ASTM s (Var a)
    -> ASTM s (Var a)
    -> ASTM s (Var a)
if_ b then_ else_ = liftF $ If b then_ else_ id

generate ::
       (Typeable a)
    => (ASTM globalState (Var (Maybe a)))
    -> ASTM globalState (Var (Generator IO a))
generate f =
    call (liftSf (sfm . pure . fmap forceDynamic)) united =<<
    liftF (Generate f id)

-- | Helper class to make 'call' a multi arity function
class CollectVars t where
    -- | As this class calls itself recursively this whitnesses that
    -- @Ret (a -> b) ~ Ret b)@ which in the end is @r@ in @... -> ASTM
    -- state (Var r)@
    type Ret t
    -- | This exists becuase we need a whitness that the @state@ type in
    -- the ASTM continuation is the same as in the ASTM value returned
    -- by @t@ in the end.
    type GlobalState t
    collectVars ::
           [Var Void] -- ^ The input values
        -> ([Var Void] -> ASTM (GlobalState t) (Var (Ret t))) -- ^ the continuation
        -> t

instance CollectVars b => CollectVars (Var a -> b) where
    type Ret (Var a -> b) = Ret b
    type GlobalState (Var a -> b) = GlobalState b
    collectVars l f v = collectVars (toVoidVar v : l) f

instance CollectVars (ASTM gs (Var (a :: *))) where
    type Ret (ASTM gs (Var a)) = a
    type GlobalState (ASTM gs (Var a)) = gs
    collectVars l f = f l

-- Evaluating the free monad
-- | Another way of referencing a stateful function. This one also
-- contains an accessor for the state
data SfRef globalState =
    forall sfType state returnType. (ReturnType sfType ~ SfMonad state returnType) =>
                                    SfRef (Sf sfType)
                                          (Accessor globalState state)

-- | This governs how a node is embedded.
-- Either a stateful function, which gets wrapped, or a stream processor which is
-- more powerful and does its own dataflow processing.
data NodeType globalState m
    = CallSf (SfRef globalState)
    | StreamProcessor (StreamInit m ())

data Node globalState m = Node
    { nodeDescription :: QualifiedBinding
    , nodeId :: FnId
    , nodeType :: NodeType m globalState
    , ctxRef :: Maybe Unique
    , inputRefs :: [Unique]
    , outputRef :: Unique
    }

data Algorithm globalState m a =
    Algorithm G.OutGraph
              (FunctionDict globalState m)

type FunctionDict s m = Map.Map QualifiedBinding (NodeType s m)

type UDict = Map.Map Unique Binding

newtype EvalASTM s m a =
    EvalASTM (RWS () (Mutator L.Expression) ( FunctionDict s m
                                            , UDict
                                            , NameGenerator
                                            , NameGenerator
                                            , Unique) a)
    deriving (Functor, Applicative, Monad, MonadWriter (Mutator L.Expression))

class FreshUnique m where
    freshUnique :: m Unique

instance FreshUnique (EvalASTM s m) where
    freshUnique = EvalASTM $ (_5 %= succ) >> use _5

evalASTM ::
       MonadStream m
    => EvalASTM s m a
    -> (FunctionDict s m, L.Expression -> L.Expression, a)
evalASTM (EvalASTM ac) = (d, m, a)
  where
    throwErrs = either (error . Str.toString) id
    (a, (d, _, _, _, _), Mutator m) =
        runRWS
            ac
            ()
            ( defaultFunctionDict
            , mempty
            , throwErrs $ initNameGen mempty
            , throwErrs $ initNameGen mempty
            , Unique 0)

instance MonadGenBnd (EvalASTM s m) where
    generateBinding = EvalASTM $ generateBindingIn _3
    generateBindingWith = EvalASTM . generateBindingWithIn _3

generateSFunctionName :: EvalASTM s m QualifiedBinding
generateSFunctionName =
    EvalASTM $ QualifiedBinding ["__generated"] <$> generateBindingIn _4

-- | Evaluate the AST Monad and ceate a graph
createAlgo :: MonadStream m => ASTM s (Var a) -> IO (Algorithm s m a)
createAlgo astm = flip Algorithm dict <$> (dump =<< runCompiler expr)
  where
    (dict, expr) = evaluateAST astm
    dump a = pure a
    dump gr = do
      BS.writeFile "algo-graph" (A.encode gr)
      error ""

registerFunction :: NodeType s m -> EvalASTM s m QualifiedBinding
registerFunction n = do
    name <-
        case n of
            CallSf (SfRef (Sf _ (Just predefinedName)) _) -> pure predefinedName
            _ -> generateSFunctionName
    EvalASTM $ _1 . at name .= Just n
    pure name

varToBnd :: Var a -> EvalASTM s m Binding
varToBnd (Var u) =
    EvalASTM $ fromMaybe (error "bindings must be defined before use") <$>
    preuse (_2 . ix u)

mkRegVar :: EvalASTM s m (Var a, Binding)
mkRegVar = do
    b <- generateBinding
    u <- freshUnique
    EvalASTM $ _2 . at u .= Just b
    pure (Var u, b)

dfFnRefName :: DFFnRef -> QualifiedBinding
dfFnRefName (EmbedSf n) = n
dfFnRefName (DFFunction n) = n

nthNS :: NSRef
nthNS = ["ohua", "lang", "nth"]

dN :: Int -> QualifiedBinding
dN i = QualifiedBinding nthNS (makeThrow $ Str.showS i)

captureSingleton :: QualifiedBinding
captureSingleton = QualifiedBinding ohuaLangNS "captureSingleton"

defaultFunctionDict :: MonadStream m => FunctionDict s m
defaultFunctionDict =
    Map.fromList $
    ( captureSingleton
    , StreamProcessor $ pure $ recieveUntyped 0 >>= \v -> sendUntyped v) :
    map (first dfFnRefName)
        [ ( DFRefs.smapFun
          , StreamProcessor $ do
                stateVar <- liftIO $ newIORef []
                pure $ do
                    l <- recieveUntyped 0
                    withIsAllowed $
                        sendUntyped =<<
                        liftIO
                            (atomicModifyIORef' stateVar $ \case
                                 [] ->
                                     case extractList l of
                                         [] -> error "new input list was empty"
                                         (x:xs) -> (xs, x)
                                 (x:xs) -> (xs, x)))
        , ( DFRefs.collect
          , StreamProcessor $
            pure $ do
                size <- recieve 0
                withIsAllowed $ do
                    void $ sequence $ replicate (pred size) $ recieveUntyped 0
                    vs <- sequence $ replicate size $ recieveUntyped 1
                    sendUntyped $ injectList vs)
        , ( DFRefs.oneToN
          , StreamProcessor $ do
                verifyInputNum 2
                pure $ do
                    size <- recieve 0
                    v <- recieveUntyped 1
                    withIsAllowed $ sequence_ $ replicate size $ sendUntyped v)
        , ( DFRefs.scope
          , StreamProcessor $
            pure $ do
                vals <- recieveAllUntyped
                withIsAllowed $ send vals)
        , ( DFRefs.bool
          , StreamProcessor $
            pure $ do
                b <- recieve 0
                withIsAllowed $ send $ V.fromList [toDyn b, toDyn $ not b])
        , ( DFRefs.select
          , StreamProcessor $
            pure $ do
                b <- recieve 0
                withIsAllowed $
                    sendUntyped =<<
                    recieveUntyped
                        (if b
                             then 1
                             else 2))
        , ( DFRefs.size
          , StreamProcessor $
            pure $ do
                coll <- recieveUntyped 0
                withIsAllowed $ send $ length (extractList coll))
        , ( DFRefs.seq
          , StreamProcessor $ do
                verifyInputNum 1
                pure $ do
                    void $ recieveUntyped 0
                    withIsAllowed $ send True)
        , ( DFRefs.id
          , StreamProcessor $ do
                verifyInputNum 1
                pure $ recieveUntyped 0 >>= withIsAllowed . sendUntyped)
        , ( DFRefs.smapGFun
          , StreamProcessor $
            pure $ do
                gen <- extractFunctor <$> recieveUntyped 0
                let gWithFlush :: Generator IO (Vector Dynamic)
                    gWithFlush =
                        fmap (V.fromList . (: [toDyn True])) (gen :: Generator IO Dynamic) `mappend`
                        [ V.fromList
                              [ toDyn (error "this needs to be dropped" :: ())
                              , toDyn False
                              ]
                        ]
                withIsAllowed $ foldlGeneratorT_ liftIO send gWithFlush)
        , ( DFRefs.collectG
          , StreamProcessor $
            pure $ do
                l@(x:_) <- whileM (recieve 0) (recieveUntyped 1)
                sendUntyped $
                    injectFunctor x $ (fromList l :: Generator IO Dynamic))
        , ( DFRefs.repeat
          , StreamProcessor $ do
                stateStore <- liftIO $ newMVar Nothing
                let recieveVals = recieveWhereUntyped (/= 0)
                pure $ do
                    withIsAllowed $ do
                        b <- recieve 0
                        if b
                            then do
                                vals <-
                                    liftIO (takeMVar stateStore) >>=
                                    maybe recieveVals pure
                                liftIO $ putMVar stateStore $ Just vals
                                send vals
                            else liftIO $
                                 modifyMVar_ stateStore (const $ pure Nothing))
        , ( DFRefs.isJust
          , StreamProcessor $
            pure $
            withIsAllowed $
            send =<< (isJust . extractFunctor <$> recieveUntyped 0))
        -- , ( DFRefs.ndMerge
        --   , StreamProcessor $
        --     pure $
        --     withIsAllowed $ do
        --         let exhaust c =
        --                 forever $
        --                 withIsAllowed $
        --                 recieveSource
        --                     (closeCtxArc >>
        --                      endProcessingAt (\i -> i /= 1 && i /= 0))
        --                     c >>=
        --                 send
        --             runOrExhaust _ (UserPacket p) =
        --                 send $ (forceDynamic p :: Bool)
        --             runOrExhaust c EndOfStreamPacket = exhaust c
        --         p1 <- getInputPort 0
        --         p2 <- getInputPort 1
        --         case (p1, p2) of
        --             (ArcPort s1@(Source c1), ArcPort s2@(Source c2)) ->
        --                 liftIO
        --                     (atomically $ (Left <$> c1) `orElse` (Right <$> c2)) >>=
        --                 either (runOrExhaust s1) (runOrExhaust s2)
        --             _ ->
        --                 error
        --                     "Non deterministic merge with constant arcs makes no sense")
        , ( DFRefs.toGen
          , StreamProcessor $ do
                chanStore <- liftIO $ newMVar Nothing
                verifyInputNum 1
                pure $ do
                    currGeneratorChan <-
                        liftIO (takeMVar chanStore) >>= \case
                            Nothing -> do
                                c <- liftIO newChan
                                send $
                                    (chanToGenerator c :: Generator IO Dynamic)
                                pure $ liftIO . writeChan c
                            Just f -> pure f
                    v <- extractFunctor <$> recieveUntyped 0
                    currGeneratorChan v
                    liftIO $
                        putMVar chanStore $
                        if isNothing v
                            then Nothing
                            else Just currGeneratorChan)
        ]

evaluateAST :: MonadStream m => ASTM s (Var a) -> (FunctionDict s m, L.Expression)
evaluateAST (ASTM m) = (dict, build e)
  where
    evalInner :: MonadStream m => ASTM s (Var a) -> EvalASTM s m L.Expression
    evalInner (ASTM inner) = do
        (e', Mutator build') <- censor (const mempty) $ listen $ iterM go inner
        build' . L.Var . L.Local <$> varToBnd e'
    (dict, build, e) =
        evalASTM $ do
            v <- iterM go m
            L.Apply (L.Var (L.Sf captureSingleton Nothing)) . L.Var . L.Local <$>
                varToBnd v
    go :: MonadStream m => ASTAction s (EvalASTM s m (Var a)) -> EvalASTM s m (Var a)
    go (InvokeSf sf tag vars cont) = do
        n <- registerFunction (CallSf $ SfRef sf tag)
        (rv, rb) <- mkRegVar
        vars' <- mapM varToBnd vars
        tellMut $
            L.Let
                (Direct rb)
                (foldl
                     L.Apply
                     (L.Var (L.Sf n Nothing))
                     (if null vars'
                          then [unitExpr]
                          else map (L.Var . L.Local) vars'))
        cont rv
    go (SmapLike (FunctorTaggedBinding refToCall) innerCont inputVar cont) = do
        (innerContVar, innerContBnd) <- mkRegVar
        e' <- evalInner $ innerCont innerContVar
        inpBnd <- varToBnd inputVar
        (smapResVar, smapResBnd) <- mkRegVar
        tellMut $
            L.Let
                (Direct smapResBnd)
                (L.Var (L.Sf refToCall Nothing) `L.Apply`
                 L.Lambda (Direct innerContBnd) e' `L.Apply`
                 L.Var (L.Local inpBnd))
        cont smapResVar
    go (If v then_ else_ cont) = do
        (rv, rb) <- mkRegVar
        vbnd <- varToBnd v
        thenE <- evalInner then_
        elseE <- evalInner else_
        tellMut $
            L.Let
                (Direct rb)
                (L.Var (L.Sf ARefs.ifThenElse Nothing) `L.Apply`
                 L.Var (L.Local vbnd) `L.Apply`
                 L.Lambda "_" thenE `L.Apply`
                 L.Lambda "_" elseE)
        cont rv
    go (Generate inner cont) = do
        (rv, rb) <- mkRegVar
        innerE <- evalInner inner
        tellMut $
            L.Let
                (Direct rb)
                (L.Var (L.Sf ARefs.generate Nothing) `L.Apply`
                 L.Lambda "_" innerE)
        cont rv


newtype GenIdM a = GenIdM
    { unwrapGenIdM :: S.State FnId a
    } deriving (Applicative, Monad, Functor)

instance MonadGenId GenIdM where
    generateId = GenIdM $ modify succ >> get
    resetIdCounter = GenIdM . put

runGenIdM :: GenIdM a -> FnId -> a
runGenIdM = evalState . unwrapGenIdM

makeDestructuringExplicit :: G.OutGraph -> G.OutGraph
makeDestructuringExplicit G.OutGraph {G.operators, G.arcs, G.returnArc} =
    G.OutGraph (operators <> ops') arcs' returnArc
  where
    (ops', arcs') = fold $ runGenIdM (mapM go arcs) largestId
    largestId = maximum (map G.operatorId operators)
    go a@G.Arc {G.source} =
        case source of
            G.LocalSource t@G.Target {G.index = i}
                | i == -1 -> pure (mempty, pure a)
                | otherwise -> do
                    opid <- generateId
                    pure
                        ( pure
                              G.Operator
                                  {G.operatorId = opid, G.operatorType = dN i}
                        , [ a
                                { G.source =
                                      G.LocalSource
                                          G.Target
                                              {G.operator = opid, G.index = -1}
                                }
                          , G.Arc
                                { G.target =
                                      G.Target {G.operator = opid, G.index = 0}
                                , G.source = G.LocalSource t {G.index = -1}
                                }
                          ])
            _ -> pure (mempty, pure a)

runCompiler :: L.Expression -> IO G.OutGraph
runCompiler =
    fmap (either (error . Str.toString) makeDestructuringExplicit) . runExceptT .
    runStderrLoggingT .
    filterLogger (const $ (>= LevelError)) .
    compile def def {passAfterDFLowering = cleanUnits}

-- The stream backend
data Packet a
    = UserPacket a
    | EndOfStreamPacket

-- | True if the packet is an 'EndOFStreamPacket'
isEOS :: Packet a -> Bool
isEOS EndOfStreamPacket = True
isEOS _ = False


-- | The monad that a stream processor runs in. It has access to a
-- number of input streams to pull from and a number of output streams
-- to send to.  Additionally IO is enabled with 'MonadIO' and a short
-- circuiting via 'abortProcessing' which stops the processing.
newtype StreamM m a = StreamM
    { runStreamM :: ExceptT (Maybe String) (ReaderT ( Maybe (Reciever m (Packet Dynamic))
                                                    , Vector (InputPort m)
                                                    , OutputPort m) m) a
    } deriving (Functor, Applicative, Monad, MonadIO)

type StreamInit m a = StreamM m (StreamM m a)

instance MonadTrans StreamM where
  lift = StreamM . lift . lift

-- getUnifiedPort :: StreamM OutputPort
-- getUnifiedPort = StreamM $ unifiedPort <$> view _3

-- getAllIndexedPorts :: StreamM (V.Vector OutputPort)
-- getAllIndexedPorts = StreamM $ indexedPorts <$> view _3

-- getIndexedPort :: Int -> StreamM OutputPort
-- getIndexedPort i = (V.! i) <$> getAllIndexedPorts

getOutputPort :: Monad m => StreamM m (OutputPort m)
getOutputPort = StreamM $ view _3

sendToPort :: MonadStream m => Packet Dynamic -> OutputPort m -> StreamM m ()
sendToPort p (OutputPort port)
    | V.null port = pure ()
    | otherwise = lift $ mapM_ (ST.send p) port

sendPacket :: MonadStream m => Packet Dynamic -> StreamM m ()
sendPacket p = getOutputPort >>= sendToPort p

-- | Send a packet to all unified output streams
-- sendPacketUnified :: Packet Dynamic -> StreamM ()
-- sendPacketUnified p = getUnifiedPort >>= sendToPort p

-- sendPacketToIndexed :: Int -> Packet Dynamic -> StreamM ()
-- sendPacketToIndexed i p = getIndexedPort i >>= sendToPort p

-- sendPacketToAllIndexed :: Packet Dynamic -> StreamM ()
-- sendPacketToAllIndexed p = getAllIndexedPorts >>= mapM_ (sendToPort p)

-- | Stop processing immediately. No subsequent actions are performed.
abortProcessing :: Monad m => StreamM m a
abortProcessing = StreamM (throwError Nothing)

packetsLeftOverErr :: Monad m => String -> StreamM m a
packetsLeftOverErr = StreamM . throwError . Just

-- | This can be used to gracefully end processing after an 'EndOfStreamPacket'
endProcessingAt :: MonadStream m => (Int -> Bool) -> StreamM m a
endProcessingAt p = do
    sendEOS -- make sure the error of having excess input does not
            -- propagate unnecessarily
    results <-
        getInputPorts >>=
        V.imapM
            (\i ->
                 if p i
                     then \case
                              ConstValPort _ -> pure Nothing
                              ArcPort s ->
                                  recievePacket s <&> \r ->
                                      if isEOS r
                                          then Nothing
                                          else Just i
                     else const $ pure Nothing)
    case foldl (<|>) Nothing results of
        Nothing -> abortProcessing
        Just i
      -- eventually we'll want to send some information here which
      -- port had data left over in it
         -> packetsLeftOverErr (show i)

withCtxArc :: Monad m => StreamM m a -> (Reciever m (Packet Dynamic) -> StreamM m a) -> StreamM m a
withCtxArc failAction succeedAction =
    StreamM (view _1) >>= maybe failAction succeedAction

closeCtxArc :: MonadStream m => StreamM m ()
closeCtxArc =
    withCtxArc (pure ()) $ \arc -> do
        ret <- recievePacket arc
        if isEOS ret
            then pure ()
            else packetsLeftOverErr "ctx arc"

endProcessing :: MonadStream m => Int -> StreamM m a
endProcessing i = closeCtxArc >> endProcessingAt (/= i)

expectFinish :: MonadStream m => StreamM m a
expectFinish = closeCtxArc >> endProcessingAt (const True)

sendEOS :: MonadStream m => StreamM m ()
sendEOS = sendPacket EndOfStreamPacket
    -- sendPacketUnified EndOfStreamPacket >>
    -- sendPacketToAllIndexed EndOfStreamPacket

recievePacket :: MonadStream m => Reciever m (Packet a) -> StreamM m (Packet a)
recievePacket = lift . ST.recieve

getInputPorts :: Monad m => StreamM m (V.Vector (InputPort m))
getInputPorts = StreamM $ view _2

getInputPort :: Monad m => Int -> StreamM m (InputPort m)
getInputPort i = (V.! i) <$> getInputPorts

recieveUntyped :: MonadStream m => Int -> StreamM m Dynamic
recieveUntyped i = getInputPort i >>= queryPort i

recieveSource :: MonadStream m => StreamM m a -> Reciever m (Packet a) -> StreamM m a
recieveSource finish =
    recievePacket >=> \case
        EndOfStreamPacket -> finish
        UserPacket u -> pure u

recieveAllUntyped :: MonadStream m => StreamM m (Vector Dynamic)
recieveAllUntyped = getInputPorts >>= V.imapM queryPort

queryPort :: MonadStream m => Int -> InputPort m -> StreamM m Dynamic
queryPort _ (ConstValPort v) = pure v
queryPort i (ArcPort p) = recieveSource (endProcessing i) p

recieveWhereUntyped :: MonadStream m => (Int -> Bool) -> StreamM m (Vector Dynamic)
recieveWhereUntyped p =
    getInputPorts >>= V.imapM queryPort . V.ifilter (const . p)

recieveAll :: (Typeable a, MonadStream m) => StreamM m (Vector a)
recieveAll = fmap forceDynamic <$> recieveAllUntyped

recieve :: (Typeable a, MonadStream m) => Int -> StreamM m a
recieve = fmap forceDynamic . recieveUntyped

verifyInputNum :: Monad m => Int -> StreamM m ()
verifyInputNum expected =
    (V.length <$> getInputPorts) >>= \i ->
        if expected == i
            then pure ()
            else error $ "Expected " ++ show expected ++ " got " ++ show i

-- TODO make this type safe at some point
send :: (Typeable a, MonadStream m) => a -> StreamM m ()
send = sendUntyped . toDyn

sendUntyped :: MonadStream m => Dynamic -> StreamM m ()
sendUntyped = sendPacket . UserPacket

withIsAllowed :: MonadStream m => StreamM m () -> StreamM m ()
withIsAllowed ac =
    withCtxArc ac $ \arc -> do
        b <- forceDynamic <$> recieveSource (endProcessingAt (const True)) arc
        when b ac

-- Running stateful functions as a Stream processor
atomicModifyIORef'_ :: MonadIO m => IORef a -> (a -> a) -> m ()
atomicModifyIORef'_ ref f = liftIO $ atomicModifyIORef' ref ((, ()) . f)

atomicModifyIORef_ :: MonadIO m => IORef a -> (a -> a) -> m ()
atomicModifyIORef_ ref f = liftIO $ atomicModifyIORef ref ((, ()) . f)

-- | A class that is used to apply a function with multiple arguments
-- to a list of arguments.
class ApplyVars t where
    applyVars :: t -> [Dynamic] -> ReturnType t

instance (ApplyVars b, Typeable a) => ApplyVars (a -> b) where
    applyVars func (x:xs) = applyVars (func $ forceDynamic x) xs
    applyVars _ [] = error "too few arguments"

instance ApplyVars (SfMonad state retType) where
    applyVars res [] = res
    applyVars _ _ = error "Too many arguments"

collectStats = False

type Stats = Map.Map QualifiedBinding Integer
type RawStats = [(QualifiedBinding, Integer)]

-- | Given a reference for a stateful function run it as a stream
-- processor
runFunc ::
       MonadStream m
    => IORef RawStats
    -> QualifiedBinding
    -> SfRef globalState
    -> globalState
    -> IO (Maybe (globalState -> IO globalState), StreamInit m ())
runFunc statRef funcName (SfRef (Sf f _) accessor) st = do
    stateRef <- liftIO $ newIORef (st ^. accessor)
    pure $
        (Just $ \s -> readIORef stateRef >>= \a -> pure $ s & accessor .~ a, ) $ pure $ do
            inVars <- recieveAllUntyped
            withIsAllowed $ do
                s <- liftIO $ readIORef stateRef
                (ret, newState) <-
                    liftIO $
                    caught $
                    measured $
                    runStateT (runSfMonad (applyVars f $ V.toList inVars)) s
                liftIO $ writeIORef stateRef newState
                ret `seq` pure ()
                send ret
  where
    measured ac
        | collectStats = do
            start <- getCPUTime
            res <- ac
            end <- getCPUTime
            atomicModifyIORef_ statRef ((funcName, end - start) :)
            pure res
        | otherwise = ac
    caught ac =
        ac `catch`
        (\e@(ErrorCall _) ->
             error $ printf "error in %v '%v'" (show funcName) (show e))



-- Running the stream
mountStreamProcessor ::
       MonadStream m
    => StreamInit m ()
    -> Maybe (Reciever m (Packet Dynamic))
    -> Vector (InputPort m)
    -> OutputPort m
    -> m ()
mountStreamProcessor process ctxInput inputs outputs = do
    result <-
        runReaderT
            (runExceptT $ runStreamM safeProc)
            (ctxInput, inputs, outputs)
    case result of
        Left packetInfo ->
            case packetInfo of
                Nothing -> pure ()
          -- EOS marker appeared, this is what *should* happen
                Just i ->
                    error $ "There were packets left over in " ++ i ++
                    " when a processor exited"
        Right () ->
            logError $ "IMPOSSIBLE: operator terminated without exception"
  where
    safeProc = do
        proc_ <- process
        if null inputs && isNothing ctxInput
            then proc_ >> expectFinish
            else forever proc_

data ExecutionException =
    ExecutionException [(G.Operator, SomeException)]
    deriving (Typeable)

prettyExecutionException :: ExecutionException -> String
prettyExecutionException (ExecutionException errors) =
    unlines
        [ Str.intercalate
            "\n  "
            ((e ++ " in ") :
             map showOp (take maxNumExceptionSources opList) ++
             [ case length opList - maxNumExceptionSources of
                   i
                       | i > 0 -> "(" ++ show i ++ " more)"
                       | otherwise -> ""
             ])
        | (e, opSet) <- Map.toList errMap
        , let opList = nub opSet
        ]
  where
    showOp G.Operator{..} = show operatorType ++ "(" ++ show operatorId ++ ")"
    maxNumExceptionSources = 5
    errMap =
        Map.fromListWith (++) $
        map (\(op, e) -> (show e, [op])) errors

instance Exception ExecutionException

instance Show ExecutionException where
    show = prettyExecutionException

data InputPort m
    = ConstValPort Dynamic
    | ArcPort (Reciever m (Packet Dynamic))

newtype OutputPort m =
    OutputPort (V.Vector (Sender m (Packet Dynamic)))

-- data OutputManager = OutputManager
--     { indexedPorts :: V.Vector OutputPort
--     , unifiedPort :: OutputPort
--     }

toOutputManager :: [(Int, Sender m (Packet Dynamic))] -> OutputPort m
toOutputManager v
    | Just _ <- IMap.lookupLT (-1) unifiedArcsMap =
        error "Cannot have target index less than -1"
    | not $ IMap.null argMap =
        error "Destructuring is not supported in the runtime yet"
    | otherwise =
        OutputPort $ maybe [] V.fromList $ IMap.lookup (-1) unifiedArcsMap
      -- this code is for later, if we choose to add destructuring to the runtime
        -- OutputManager
        --     (fmap V.fromList $
        --      V.unfoldr
        --          (\i ->
        --               fmap (, succ i) $
        --               IMap.lookup i argMap <|>
        --               if i > fst (IMap.findMax argMap)
        --                   then Nothing
        --                   else Just [])
        --          0)
        --     (maybe [] V.fromList $ IMap.lookup (-1) unifiedArcsMap)
  where
    (argMap, unifiedArcsMap) =
        IMap.partitionWithKey (\k _ -> k > 0) $
        IMap.fromListWith (++) $ fmap (second pure) v

constants :: Map.Map HostExpr Dynamic
constants = [(HEConst.true, toDyn True)]

formatStats :: [(QualifiedBinding, Integer)] -> Stats
formatStats =
    fmap
     -- sum
     (\(xs::[Integer]) -> round $ realToFrac (sum xs) / genericLength xs)
     . Map.fromListWith (++)
     . map (second pure)

runAlgo ::
       forall m a globalState. (MonadStream m, Typeable a)
    => Algorithm globalState m a
    -> globalState
    -> m a
runAlgo a st = fst <$> runAlgoWStats a st

runAlgoWStats ::
       forall m a globalState. (MonadStream m, Typeable a)
    => Algorithm globalState m a
    -> globalState
    -> m (a, Stats)
runAlgoWStats (Algorithm g@G.OutGraph {..} dict) st = do
    statRef <- liftIO $ newIORef []
    (outMap, funcs) <- foldM f (outputMap, []) operators
    (retSink, retSource) <- ST.createStream
    let finalMap =
            fmap toOutputManager $
            Map.update
                (Just . ((G.index returnArc, retSink) :))
                (G.operator returnArc)
                outMap
    stateRecovery <- catMaybes . fst . unzip <$>
        mapM
            (\(op@(G.Operator {..}), ctxArc, inChans) ->
                 let outChans = finalMap Map.! operatorId
                     operatorCode
                         | nthNS == qbNamespace operatorType =
                             StreamProcessor $
                             pure $ do
                                 input <- recieve 0
                                 sendUntyped $
                                     input V.!
                                     read
                                         (Str.toString . unwrap $
                                          qbName operatorType)
                         | otherwise =
                             fromMaybe
                                 (error $ "cannot find " ++ show operatorType) $
                             Map.lookup operatorType dict

                     initProc =
                         case operatorCode of
                             CallSf sfRef -> runFunc statRef operatorType sfRef st
                             StreamProcessor processor' -> pure (Nothing , processor')
                 in do
                   liftIO $ putStrLn $ "fn: " ++ (show operatorType) 
                   (syncState, processor) <- liftIO initProc
                   (syncState, ) <$>
                     ST.spawn
                         (mountStreamProcessor
                              processor
                              ctxArc
                              (V.fromList inChans)
                              outChans))
            funcs
    UserPacket ret <- ST.recieve retSource
    EndOfStreamPacket <- ST.recieve retSource
    stats <- liftIO $ readIORef statRef
    pure (forceDynamic ret, formatStats stats)
  where
    outputMap :: Map.Map FnId [(Int, Sender m (Packet Dynamic))]
    outputMap = Map.fromList $ zip (map G.operatorId operators) (repeat [])
    f (m, l) op@G.Operator {operatorId} = do
        let (ctxInput, inputs) =
                fromMaybe (Nothing, []) $ Map.lookup operatorId argDict
        arcs <-
            forM inputs $ \case
                G.LocalSource t -> do
                    (sink, source) <- ST.createStream
                    pure $ Left ((t, sink), source)
                G.EnvSource v -> pure $ Right v
        let mapUpdates = map fst $ lefts arcs
            inputPorts =
                map
                    (either (ArcPort . snd) (ConstValPort . (constants Map.!)))
                    (arcs :: [Either ( (G.Target, Sender m (Packet Dynamic))
                                     , Reciever m (Packet Dynamic)) HostExpr])
            newMap =
                foldl
                    (\theMap (G.Target {G.index, G.operator}, chan) ->
                         Map.update (Just . ((index, chan) :)) operator theMap)
                    m
                    mapUpdates
        (newMap', ctxArc) <-
            case ctxInput of
                Just G.Target {G.index, G.operator} -> do
                    (sink, source) <- ST.createStream
                    pure
                        ( Map.update (Just . ((index, sink) :)) operator newMap
                        , Just source)
                Nothing -> pure (newMap, Nothing)
        pure (newMap', (op, ctxArc, inputPorts) : l)
    argDict :: Map.Map FnId (Maybe G.Target, [G.Source HostExpr])
    argDict = fmap partitionArgs $ Map.fromListWith (++) $ fmap simplifyArc arcs
    simplifyArc G.Arc {target = G.Target {G.index, G.operator}, source} =
        (operator, [(index, source)])
    partitionArgs l =
        ( (\case
               G.EnvSource _ -> error "Ctx src with env source makes no sense"
               G.LocalSource l -> l) .
          snd <$>
          find ((== -1) . fst) l
        , map snd $ sortOn fst $ filter ((/= -1) . fst) l)


-- Types for the example
data T =
    T
    deriving (Show)

-- Stateful functions for the example
stag :: Accessor s ()
stag = united

sf1 :: ASTM s (Var T)
sf1 = call (liftSf (sfm $ liftIO (putStrLn "Executing sf1") >> pure T)) stag

sf2 :: Var T -> ASTM s (Var [Int])
sf2 =
    call
        (liftSf
             (\T -> sfm $ liftIO (putStrLn "Executing sf2") >> return [0 .. 20]))
        stag

sf3 :: Var T -> Var Int -> ASTM s (Var Int)
sf3 =
    call
        (liftSf
             (\T i -> sfm $ liftIO (putStrLn "Executing sf3") >> return (succ i)))
        stag

aggregate :: Var Int -> ASTM Int (Var Int)
aggregate =
    call
        (liftSf
             (\i ->
                  sfm $ liftIO (putStrLn $ "Executing aggregate " ++ show i) >>
                  S.modify (+ i) >>
                  S.get))
        (lens id (const id))

plus :: Var Int -> Var Int -> ASTM s (Var Int)
plus = call (liftSf (\a b -> sfm $ pure $ a + b)) stag

sfConst :: Typeable a => a -> ASTM s (Var a)
sfConst a = call (liftSf (sfm $ pure a)) stag

gt :: (Typeable a, Ord a) => Var a -> Var a -> ASTM s (Var Bool)
gt = call (liftSf (\a b -> sfm $ pure $ a > b)) stag

algorithm :: MonadStream m => IO (Algorithm Int m [Int])
algorithm =
    createAlgo  (do
        var1 <- sf1
        var3 <- sf2 var1
        var2 <- sfConst 3
        v <-
            smap
                (\v ->
                     aggregate =<< do
                         isGreater <- (gt v var2)
                         if_ isGreater (pure v) (plus v var2))
                var3
        pure v)
