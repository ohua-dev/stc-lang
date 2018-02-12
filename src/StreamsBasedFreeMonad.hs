{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module StreamsBasedFreeMonad where


import           Control.Monad
import           Control.Monad.Except
-- import           Control.Monad.Par        as P
import           Control.Arrow            (first, (&&&), (***))
import           Control.Monad.Reader
import           Control.Monad.RWS        as RWS
import           Control.Monad.State      as S
--
-- for debugging only:
-- import Scheduler as P
-- import Control.DeepSeq
--
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Monad.Free
-- import           Control.Parallel         (pseq)
import           Data.Default.Class
import           Data.Dynamic2
import           Data.Foldable            (fold)
import           Data.IORef
import           Data.Kind
import           Data.List                (find, sortOn)
import           Data.Map                 ((!))
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Tuple.OneTuple
import           Data.Typeable
import           Data.Void
import           Debug.Trace
import           Lens.Micro
import           Lens.Micro.Mtl
import qualified Ohua.ALang.Lang          as L
import qualified Ohua.ALang.Refs          as ARefs
import           Ohua.Compile
import qualified Ohua.DFGraph             as G
import           Ohua.DFLang.Lang         (DFFnRef (..))
import qualified Ohua.DFLang.Refs         as DFRefs
import           Ohua.Monad
import           Ohua.ParseTools.Refs     (ohuaLangNS)
import           Ohua.Types
import           Ohua.Unit
import qualified Ohua.Util.Str            as Str
import           Unsafe.Coerce

#if __GLASGOW_HASKELL__ >= 802
import qualified Type.Reflection          as Refl
import qualified Type.Reflection.Unsafe   as Refl
#endif



#if __GLASGOW_HASKELL__ >= 802
mkTyConApp :: TyCon -> [TypeRep] -> TypeRep
mkTyConApp con args = Refl.SomeTypeRep $ Refl.mkTrCon con args
#endif
-- Utils


-- | Coerce a dynamic to a value.
-- If the expected type is not the one inside the 'Dynamic' it throws an error showing both types.
forceDynamic :: forall a . Typeable a => Dynamic -> a
forceDynamic dyn
  | Just a <- fromDynamic dyn = a
  | otherwise = throw $ TypeCastException rep (dynTypeRep dyn)
  where rep = typeRep (Proxy :: Proxy a)


data TypeCastException = TypeCastException TypeRep TypeRep
  deriving (Typeable, Show)

instance Exception TypeCastException where
  displayException (TypeCastException expected recieved) =
    "TypeCastexception: Expected " ++ show expected ++ " got " ++ show recieved

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


united :: Lens' s ()
united f s = const s <$> f ()

-- The free monad


-- | A unique thing
data Unique = Unique { uniqueToInt :: Int } deriving (Ord, Eq)


instance Enum Unique where
  fromEnum = uniqueToInt
  toEnum = Unique

-- | A type tagged tracker for where data flows inside the program
newtype Var t = Var { unwrapVar :: Unique }


-- | The monad for the stateful function to run in
newtype SfMonad sfState ret = SfMonad { runSfMonad :: StateT sfState IO ret }
  deriving (Typeable, Monad, Applicative, Functor, MonadIO, MonadState sfState)


-- | A stateful funtion nicely packaged with all its required capabilities
data Sf fnType
    = forall state returnType .
    ( ReturnType fnType ~ SfMonad state returnType
    , Typeable returnType
    , ApplyVars fnType
    ) => Sf fnType -- reference to the actual function


-- | A way to retrieve and update local state in a larger state structure
type Accessor s a = Lens' s a


-- | Things you can do in the AST Monad
data ASTAction globalState a
  = forall sfType returnType sfState
    . ( ReturnType sfType ~ SfMonad sfState returnType )
    => InvokeSf
         (Sf sfType)
         (Accessor globalState sfState)
         [Var Void] -- reference to the input vars
         (Var returnType -> a) -- continutation
  | forall inputType returnType
    . (Typeable inputType, Typeable returnType)
    => Smap
         (Var inputType -> ASTM globalState (Var returnType))
         (Var [inputType])
         (Var [returnType] -> a)
  | forall returnType
    . Typeable returnType
    => If
         (Var Bool)
         (ASTM globalState (Var returnType))
         (ASTM globalState (Var returnType))
         (Var returnType -> a)

-- | The AST Monad
newtype ASTM globalState a = ASTM (Free (ASTAction globalState) a)
  deriving (MonadFree (ASTAction globalState), Monad, Applicative, Functor)


instance Functor (ASTAction globalState) where
  fmap f (InvokeSf sf tag vars ac) = InvokeSf sf tag vars (f . ac)
  fmap f (Smap f1 v cont)          = Smap f1 v (f . cont)
  fmap f (If v th el cont)         = If v th el (f . cont)


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


-- | Make a stateful function from some arbitrary function of type @f@
--
-- @f@ is not required to have any arguments but must return in the 'SfMonad'
liftSf :: ( ReturnType f ~ SfMonad state ret
          , Typeable ret
          , ApplyVars f)
       => f -> Sf f
liftSf f = Sf f


-- | A convenience function.
-- It doesn't actually do anything (@sfm = id@) but it has a concrete type and therefore
-- can be used to help the compiler when it infers the type of a function pased to 'liftSf'.
--
-- Because of the type level computation used on the function type the functions
-- often have to be annotated like so @liftSf (\\... -> ... :: SfMonad t0 t1)@
-- or otherwise the compiler is unable to infer the type.
-- The same can be achieved with this function @liftSf (\\... -> sfm $ ...)@.
sfm :: SfMonad s a -> SfMonad s a
sfm = id


-- | Map a function type by wrapping each argument in a type with kind @* -> *@
type family MapFnType (f :: Type -> Type) t where
  MapFnType f (a -> b) = f a -> MapFnType f b
  MapFnType _ b = b

-- | Set the return type of function @f@ to @t@
type family SetReturnType f t where
  SetReturnType f (a -> b) = a -> SetReturnType f b
  SetReturnType f _ = f


-- | "Call" a stateful function.
-- This takes a lifted function (see 'liftSf'), links it with an accessor for the
-- state that this function uses and produces a function that can be used in 'ASTM'
call :: ( newFnType ~ SetReturnType (ASTM globalState (Var ret)) (MapFnType Var f)
        , ReturnType f ~ SfMonad state ret
        , Ret newFnType ~ ret
        , GlobalState newFnType ~ globalState
        , CollectVars newFnType
        ) => Sf f -> Accessor globalState state -> newFnType
call sf accessor = collectVars [] $ \vars -> liftF $ InvokeSf sf accessor (reverse vars) id

-- | Goodbye typesafety :(
toVoidVar :: Var a -> Var Void
toVoidVar (Var u) = Var u

-- | The smap primitive. Applies an ohua program to each value from a collection.
-- The semantics guarantee that the values are processed in-order with respects to each
-- node in the subgraph.
smap :: (Typeable a, Typeable b)
     => (Var a -> ASTM globalState (Var b))
     -> Var [a]
     -> ASTM globalState (Var [b])
smap f lref = liftF $ Smap f lref id

if_ :: Typeable a
    => Var Bool
    -> ASTM s (Var a)
    -> ASTM s (Var a)
    -> ASTM s (Var a)
if_ b then_ else_ = liftF $ If b then_ else_ id

-- | Helper class to make 'call' a multi arity function
class CollectVars t where
  -- | As this class calls itself recursively this whitnesses that
  -- @Ret (a -> b) ~ Ret b)@ which in the end is @r@ in @... -> ASTM state (Var r)@
  type Ret t
  -- | This exists becuase we need a whitness that the @state@ type in
  -- the ASTM continuation is the same as in the ASTM value returned by @t@ in the end.
  type GlobalState t
  collectVars :: [Var Void] -- ^ The input values
              -> ([Var Void] -> ASTM (GlobalState t) (Var (Ret t))) -- ^ the continuation
              -> t

instance (Typeable a, CollectVars b) => CollectVars (Var a -> b) where
  type Ret (Var a -> b) = Ret b
  type GlobalState (Var a -> b) = GlobalState b
  collectVars l f v = collectVars (toVoidVar v : l) f

instance CollectVars (ASTM gs (Var a)) where
  type Ret (ASTM gs (Var a)) = a
  type GlobalState (ASTM gs (Var a)) = gs
  collectVars l f = f l



-- Evaluating the free monad



-- | Another way of referencing a stateful function. This one also contains an accessor for the state
data SfRef globalState
  = forall sfType state returnType
  . (ReturnType sfType ~ SfMonad state returnType)
  => SfRef (Sf sfType) (Accessor globalState state)


-- | This governs how a node is embedded.
-- Either a stateful function, which gets wrapped, or a stream processor which is
-- more powerful and does its own dataflow processing.
data NodeType globalState
  = CallSf (SfRef globalState)
  | StreamProcessor (StreamInit ())


data Node globalState = Node
  { nodeDescription :: QualifiedBinding
  , nodeType        :: NodeType globalState
  , inputRefs       :: [Unique]
  , outputRef       :: Unique
  }

data Algorithm globalState a = Algorithm [Node globalState] Unique



type FunctionDict s = Map.Map QualifiedBinding (NodeType s)

type UDict = Map.Map Unique Binding

-- | This type onlt exists to overwrite the implementation of 'Monoid' for functions.
-- It changes 'mappend' to be '(.)' which enabes me to use this Monoid in the 'EvalASTM' as
-- writer.
newtype Mutator a = Mutator { mutAsFn :: a -> a }

instance Monoid (Mutator a) where
  mempty = Mutator id
  Mutator m1 `mappend` Mutator m2 = Mutator $ m1 . m2

newtype EvalASTM s a = EvalASTM
  { runEvalASTM :: RWS
                     ()
                     (Mutator L.Expression)
                     ( FunctionDict s
                     , UDict
                     , NameGenerator
                     , NameGenerator
                     , Unique
                     )
                     a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter (Mutator L.Expression)
             )

class FreshUnique m where
  freshUnique :: m Unique

instance FreshUnique (EvalASTM s) where
  freshUnique = EvalASTM $ (_5 %= succ) >> use _5

tellMut :: MonadWriter (Mutator a) m => (a -> a) -> m ()
tellMut = tell . Mutator

evalASTM :: EvalASTM s a -> (FunctionDict s, L.Expression -> L.Expression, a)
evalASTM (EvalASTM ac) = (d, m, a)
  where
    (a, (d, _, _, _, _), Mutator m) = runRWS ac () ( defaultFunctionDict
                                                   , mempty
                                                   , initNameGen mempty
                                                   , initNameGen mempty
                                                   , Unique 0
                                                   )

instance MonadGenBnd (EvalASTM s) where
  generateBinding = EvalASTM $ generateBindingIn _3
  generateBindingWith = EvalASTM . generateBindingWithIn _3

generateSFunctionName :: EvalASTM s QualifiedBinding
generateSFunctionName = EvalASTM $ QualifiedBinding ["__generated"] <$> generateBindingIn _4

-- | Evaluate the AST Monad and ceate a graph
createAlgo :: ASTM s (Var a) -> IO (Algorithm s a)
createAlgo astm = graphToAlgo dict <$> runCompiler expr
  where
    (dict, expr) = evaluateAST astm

registerFunction :: NodeType s -> EvalASTM s QualifiedBinding
registerFunction n = do
  name <- generateSFunctionName
  EvalASTM $ _1 . at name .= Just n
  pure name

varToBnd :: Var a -> EvalASTM s Binding
varToBnd (Var u) = EvalASTM $ fromMaybe (error "bindings must be defined before use") <$> preuse (_2 . ix u)

mkRegVar :: EvalASTM s (Var a, Binding)
mkRegVar = do
  b <- generateBinding
  u <- freshUnique
  EvalASTM $ _2 . at u .= Just b
  pure (Var u, b)

dfFnRefName :: DFFnRef -> QualifiedBinding
dfFnRefName (EmbedSf n)    = n
dfFnRefName (DFFunction n) = n

d0, d1, d2, d3, d4 :: QualifiedBinding
d0 = QualifiedBinding ohuaLangNS "fst"
d1 = QualifiedBinding ohuaLangNS "snd"
d2 = QualifiedBinding ohuaLangNS "trd"
d3 = QualifiedBinding ohuaLangNS "frth"
d4 = QualifiedBinding ohuaLangNS "fth"

dN :: Int -> QualifiedBinding
dN 0 = d0
dN 1 = d1
dN 2 = d2
dN 3 = d3
dN 4 = d4
dN _ = error "out of bounds"

captureSingleton :: QualifiedBinding
captureSingleton = QualifiedBinding ohuaLangNS "captureSingleton"


defaultFunctionDict :: FunctionDict s
defaultFunctionDict = Map.fromList $
  (captureSingleton, StreamProcessor $ pure $ recieveUntyped 0 >>= \v -> liftIO (putStrLn "recieved a final value") >> sendUntyped v)
  : map (first dfFnRefName)
    [ (DFRefs.smapFun, StreamProcessor $ do
          stateVar <- liftIO $ newIORef []
          pure $ do
            l <- recieveUntyped 0
            sendUntyped =<<
              liftIO (atomicModifyIORef' stateVar
                      $ \case
                           [] -> case extractList l of
                                   []     -> error "new input list was empty"
                                   (x:xs) -> (xs, x)
                           (x:xs) -> (xs, x))
      )
    , (DFRefs.collect, StreamProcessor $ pure $ do
          size <- recieve 0
          liftIO $ putStrLn "Recieved a size"
          sequence $ replicate (pred size) $ recieveUntyped 0
          vs <- sequence $ replicate size $ recieveUntyped 1
          liftIO $ putStrLn "Collecting done"
          sendUntyped $ injectList vs)
    , (DFRefs.oneToN, StreamProcessor $ pure $ do
          size <- recieve 0
          v <- recieveUntyped 1
          sequence_ $ replicate size $ sendUntyped v)
    , (DFRefs.scope, StreamProcessor $ pure $ do
          (b:vals) <- recieveAllUntyped
          if forceDynamic b
            then sendUntyped $ lToTup vals
            else pure ())
    , (DFRefs.bool, StreamProcessor $ pure $ do
          b <- recieve 0
          send (b, not b))
    , (DFRefs.select, StreamProcessor $ pure $ do
          b <- recieve 0
          sendUntyped =<< recieveUntyped (if b then 1 else 2))
    , (DFRefs.size, StreamProcessor $ pure $ do
          coll <- recieveUntyped 0
          send $ length (extractList coll))
    , (DFRefs.id, StreamProcessor $ pure $ recieveUntyped 0 >>= sendUntyped)
    ]
  ++ map (dN &&& destructOp) [0..4]
  where
    destructOp n = StreamProcessor $ pure $ recieveUntyped 0 >>= sendUntyped . destructureTuple n


evaluateAST :: ASTM s (Var a) -> (FunctionDict s, L.Expression)
evaluateAST (ASTM m) = (dict, build e)
  where
    evalInner :: ASTM s (Var a) -> EvalASTM s L.Expression
    evalInner (ASTM inner) = do
      (e, Mutator build) <- censor (const mempty) $ listen $ iterM go inner
      build . L.Var . L.Local <$> varToBnd e
    (dict, build, e) = evalASTM $ do
      v <- iterM go m
      L.Apply (L.Var (L.Sf captureSingleton Nothing)) . L.Var . L.Local <$> varToBnd v
    go :: ASTAction s (EvalASTM s (Var a)) -> EvalASTM s (Var a)
    go (InvokeSf sf tag vars cont) = do
      n <- registerFunction (CallSf $ SfRef sf tag)
      (rv, rb) <- mkRegVar
      vars' <- mapM varToBnd vars
      tellMut $ L.Let (Direct rb) (foldl L.Apply (L.Var (L.Sf n Nothing)) (if null vars' then [unitExpr] else map (L.Var . L.Local) vars'))
      cont rv
    go (Smap
         (innerCont :: Var inputType -> ASTM globalState (Var returnType))
         inputVar
         cont) = do
      (innerContVar, innerContBnd) <- mkRegVar
      e <- evalInner $ innerCont innerContVar
      inpBnd <- varToBnd inputVar
      (smapResVar, smapResBnd) <- mkRegVar
      tellMut $ L.Let (Direct smapResBnd) (L.Var (L.Sf ARefs.smap Nothing)
                                           `L.Apply` L.Lambda (Direct innerContBnd) e
                                           `L.Apply` L.Var (L.Local inpBnd)
                                          )
      cont smapResVar
    go (If v then_ else_ cont) = do
      (rv, rb) <- mkRegVar
      vbnd <- varToBnd v
      thenE <- evalInner then_
      elseE <- evalInner else_
      tellMut $ L.Let (Direct rb) (L.Var (L.Sf ARefs.ifThenElse Nothing)
                                  `L.Apply` L.Var (L.Local vbnd)
                                  `L.Apply` L.Lambda "_" thenE
                                  `L.Apply` L.Lambda "_" elseE
                                  )
      cont rv

newtype GenIdM a = GenIdM { unwrapGenIdM :: S.State FnId a }
  deriving (Applicative, Monad, Functor)

instance MonadGenId GenIdM where
  generateId = GenIdM $ modify succ >> get
  resetIdCounter = GenIdM . put

runGenIdM :: GenIdM a -> FnId -> a
runGenIdM = evalState . unwrapGenIdM

makeDestructuringExplicit :: G.OutGraph -> G.OutGraph
makeDestructuringExplicit G.OutGraph{G.operators, G.arcs, G.returnArc} = G.OutGraph (operators <> ops') arcs' returnArc
  where
    (ops', arcs') = fold $ runGenIdM (mapM go arcs) largestId
    largestId = maximum (map G.operatorId operators)
    go a@G.Arc{G.source, G.target} =
      case source of
        G.LocalSource t@G.Target{G.index=i}
          | i == -1 -> pure (mempty, pure a)
          | otherwise -> do
              opid <- generateId
              pure ( pure G.Operator{G.operatorId=opid, G.operatorType=dN i}
                   , [ a { G.source = G.LocalSource G.Target{ G.operator = opid
                                                            , G.index = -1
                                                            }
                         }
                     , G.Arc { G.target = G.Target{ G.operator = opid
                                                  , G.index = 0
                                                  }
                             , G.source = G.LocalSource t { G.index = -1 }
                             }
                     ]
                   )
        _ -> pure (mempty, pure a)


graphToAlgo :: FunctionDict s -> G.OutGraph -> Algorithm s a
graphToAlgo dict G.OutGraph{..} = Algorithm (map (uncurry buildOp) opWRet) lastArg
  where
    buildOp G.Operator{..} = Node operatorType
                                  (fromMaybe (error $ "cannot find " ++ show operatorType) $ Map.lookup operatorType dict)
                                  (fromMaybe [] $ Map.lookup operatorId argDict)
    opWRet = zip operators [Unique 0..]
    retDict = Map.fromList $ map (first G.operatorId) opWRet
    lastArg = fromMaybe (error "no capture op") $ lookup captureSingleton $ map (first G.operatorType) opWRet
    -- The `sortOn` here is dangerous. it relies on there being *exactly one* input present for every
    -- argument to the function. If that invariant is broken we will not detect it here but get runtime errors
    argDict = fmap (map snd . sortOn fst) $ Map.fromListWith (++) $ map arcToUnique arcs
    arcToUnique G.Arc{G.target=G.Target{G.operator, G.index=tindex}, source=src}
      = ( operator
        , case src of
            G.LocalSource G.Target{..}
              | index /= -1 -> error "invariant broken, destructuring not permitted yet"
              | otherwise -> pure (tindex, retDict ! operator)
            _ -> error "invariant broken, we dont have env args"
        )

runCompiler :: L.Expression -> IO G.OutGraph
runCompiler
  = fmap (either (error . Str.toString) makeDestructuringExplicit)
  . runExceptT
  . runStderrLoggingT
  . compile def def { passAfterDFLowering = cleanUnits }

-- The stream backend


data Packet a
    = UserPacket a
    | EndOfStreamPacket


-- | True if the packet is an 'EndOFStreamPacket'
isEOS :: Packet a -> Bool
isEOS EndOfStreamPacket = True
isEOS _                 = False

-- | A recieve end of a communication channel
newtype Source a = Source { unSource :: IO (Packet a) }

-- | A send end of a communication channel
newtype Sink a = Sink { unSink :: Packet a -> IO () }

-- | The monad that a stream processor runs in.
-- It has access to a number of input streams to pull from
-- and a number of output streams to send to.
-- Additionally IO is enabled with 'MonadIO' and a short circuiting via 'abortProcessing'
-- which stops the processing.
newtype StreamM a = StreamM
  { runStreamM :: ExceptT (Maybe ()) (ReaderT ([Source Dynamic], [Sink Dynamic]) IO) a
  } deriving (Functor, Applicative, Monad, MonadIO)

type StreamInit a = StreamM (StreamM a)

-- | Create a stream with a end that can only be sent to and one that can only be read from.
createStream :: IO (Sink a, Source a)
createStream = (Sink . writeChan &&& Source . readChan) <$> newChan

-- | Send a packet to all output streams
sendPacket :: Packet Dynamic -> StreamM ()
sendPacket p = StreamM $ liftIO . mapM_ (($ p) . unSink) =<< asks snd

-- | Stop processing immediately. No subsequent actions are performed.
abortProcessing :: StreamM a
abortProcessing = StreamM (throwError Nothing)

-- | This can be used to gracefully end processing after an 'EndOfStreamPacket'
endProcessingAt :: (Int -> Bool) -> StreamM a
endProcessingAt p = do
  numChans <- StreamM $ asks (length . fst)
  vals <- mapM (recievePacket <=< getSource) [x | x <- [0..numChans - 1], p x]
  sendEOS -- make sure the error of having excess input does not propagate unnecessarily
  if all isEOS vals
    then abortProcessing
    else
      -- eventually we'll want to send some information here which port had data left over in it
      StreamM $ throwError (Just ())

endProcessing :: Int -> StreamM a
endProcessing = endProcessingAt . (/=)

expectFinish :: StreamM a
expectFinish = endProcessingAt $ const True

sendEOS :: StreamM ()
sendEOS = sendPacket EndOfStreamPacket

recievePacket :: Source a -> StreamM (Packet a)
recievePacket = StreamM . liftIO . unSource

getSource :: Int -> StreamM (Source Dynamic)
getSource i = StreamM $ asks $ (!! i) . fst

recieveUntyped :: Int -> StreamM Dynamic
recieveUntyped i =
  getSource i >>= recievePacket >>= \case
    EndOfStreamPacket -> endProcessing i
    UserPacket u      -> return u

recieveAllUntyped :: StreamM [Dynamic]
recieveAllUntyped = StreamM (asks (length . fst)) >>= mapM recieveUntyped . enumFromTo 0 . pred

recieve :: Typeable a => Int -> StreamM a
recieve = fmap forceDynamic . recieveUntyped

-- TODO make this type safe at some point
send :: Typeable a => a -> StreamM ()
send = sendUntyped . toDyn

sendUntyped :: Dynamic -> StreamM ()
sendUntyped = sendPacket . UserPacket


-- Running stateful functions as a Stream processor


atomicModifyIORef'_ :: MonadIO m => IORef a -> (a -> a) -> m ()
atomicModifyIORef'_ ref f = liftIO $ atomicModifyIORef' ref ((, ()) . f)


-- | A class that is used to apply a function with multiple arguments to a
-- list of arguments.
class ApplyVars t where
  applyVars :: t -> [Dynamic] -> ReturnType t


instance (ApplyVars b, Typeable a) => ApplyVars (a -> b) where
  applyVars func (x:xs) = applyVars (func $ forceDynamic x) xs
  applyVars _ []        = error "too few arguments"


instance ApplyVars (SfMonad state retType) where
  applyVars res [] = res
  applyVars _ _    = error "Too many arguments"


-- | Given a reference for a stateful function run it as a stream processor
runFunc :: SfRef globalState
        -> IORef globalState
        -> StreamInit ()
runFunc (SfRef (Sf f) accessor) stTrack = pure $ do
  inVars <- recieveAllUntyped
  s <- liftIO $ readIORef stTrack
  (ret, newState) <- liftIO $ runStateT (runSfMonad (applyVars f inVars)) (s ^. accessor)
  atomicModifyIORef'_ stTrack (accessor .~ newState)
  send ret


-- Running the stream


mountStreamProcessor :: QualifiedBinding -> StreamInit () -> [Source Dynamic] -> [Sink Dynamic] -> IO ()
mountStreamProcessor name process inputs outputs = do
  result <- runReaderT (runExceptT $ runStreamM safeProc) (inputs, outputs)
  case result of
    Left Nothing  -> putStrLn $ show name ++ " finshed gracefully" -- EOS marker appeared, this is what *should* happen
    Left (Just _) -> putStrLn $ show name ++ " finished with leftover packets" --error "There were packets left over when a processor exited"
    Right ()      -> error "impossible"
  where
    safeProc = do
      proc_ <- process
      if null inputs
        then proc_ >> expectFinish
        else forever proc_


customAsync :: IO a -> IO (Async a)
customAsync thing = async $ thing `catch` \e -> do
  putStrLn (displayException (e :: SomeException))
  throwIO e


runAlgo :: Typeable a => Algorithm globalState a -> globalState -> IO a
runAlgo (Algorithm nodes retVar) st = do
  stateVar <- newIORef st
  let outputMap :: Map.Map Unique [Sink Dynamic]
      outputMap = Map.fromList $ zip (map outputRef nodes) (repeat [])
      f (m, l) (Node name func inputs oUnique) = do
        (sinks, sources) <- unzip <$> mapM (const createStream) inputs
        let newMap = foldl (\m (u, chan) -> Map.update (Just . (chan:)) u m) m (zip inputs sinks)
        pure (newMap, (func, name, sources, oUnique):l)
  (outMap, funcs) <- foldM f (outputMap, []) nodes

  (retSink, Source retSource) <- createStream

  let finalMap = Map.update (Just . (retSink:)) retVar outMap

  bracket
    (mapM (customAsync . \(nt, name, inChans, retUnique :: Unique) ->
      let outChans = fromMaybe (error "return value not found") $ Map.lookup retUnique finalMap
          processor =
            case nt of
              CallSf sfRef              -> runFunc sfRef stateVar
              StreamProcessor processor -> processor
      in mountStreamProcessor name processor inChans outChans)
      funcs)
    ( mapM_ cancel )
    $ \threads -> do
        mapM_ link threads
        UserPacket ret <- retSource
        EndOfStreamPacket <- retSource
        pure $ forceDynamic ret


-- Inspect the graph



printGraph :: Algorithm s r -> IO ()
printGraph (Algorithm gr ret) = do
  forM_ gr $ \(Node name nt vars sfRet) ->
    let ntStr = case nt of
                    CallSf _          -> "Sf"
                    StreamProcessor _ -> "StreamProcessor"
    in
    putStrLn $ show name ++ " " ++  ntStr ++ " with inputs " ++ show (map uniqueToInt vars) ++ " returns " ++ show (uniqueToInt sfRet)
  putStrLn $ "last return is " ++ show (uniqueToInt ret)


-- Types for the example


data T = T deriving (Show)


-- Stateful functions for the example


stag :: Accessor s ()
stag = united

sf1 :: ASTM s (Var T)
sf1 = call (liftSf (sfm $ liftIO (putStrLn "Executing sf1") >> pure T)) stag

sf2 :: Var T -> ASTM s (Var [Int])
sf2 = call (liftSf (\T -> sfm $ liftIO (putStrLn "Executing sf2") >> return [0..20])) stag

sf3 :: Var T -> Var Int -> ASTM s (Var Int)
sf3 = call (liftSf (\T i -> sfm $ liftIO (putStrLn "Executing sf3") >> return (succ i))) stag

aggregate :: Var Int -> ASTM Int (Var Int)
aggregate = call (liftSf (\i -> sfm $ liftIO (putStrLn $ "Executing aggregate " ++ show i) >> S.modify (+ i) >> S.get)) (lens id (const id))


plus :: Var Int -> Var Int -> ASTM s (Var Int)
plus = call (liftSf (\a b -> sfm $ pure $ a + b)) stag

sfConst :: Typeable a => a -> ASTM s (Var a)
sfConst a = call (liftSf (sfm $ pure a)) stag

gt :: (Typeable a, Ord a) => Var a -> Var a -> ASTM s (Var Bool)
gt = call (liftSf (\a b -> sfm $ pure $ a > b)) stag

algorithm :: IO (Algorithm Int [Int])
algorithm = createAlgo $ do
  var1 <- sf1
  var3 <- sf2 var1
  var2 <- sfConst 3
  v <- smap (\v -> aggregate =<< do
                 isGreater <- (gt v var2)
                 if_ isGreater (pure v) (plus v var2)) var3
  pure v
