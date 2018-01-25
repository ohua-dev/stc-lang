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
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module StreamsBasedFreeMonad where


import           Control.Monad
import           Control.Monad.Except
-- import           Control.Monad.Par        as P
import           Control.Arrow            (first)
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
import           Data.Dynamic
import           Data.IORef
import           Data.Kind
import           Data.List                (sortOn)
import           Data.Map                 ((!))
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Void
import           Debug.Trace
import           Lens.Micro
import           Lens.Micro.Mtl
import qualified Ohua.ALang.Lang          as L
import qualified Ohua.DFGraph             as G
import           Ohua.Monad
import           Ohua.Types


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

-- | The AST Monad
newtype ASTM globalState a = ASTM (Free (ASTAction globalState) a)
  deriving (MonadFree (ASTAction globalState), Monad, Applicative, Functor)


instance Functor (ASTAction globalState) where
  fmap f (InvokeSf sf tag vars ac) = InvokeSf sf tag vars (f . ac)
  fmap f (Smap f1 v cont)          = Smap f1 v (f . cont)


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
  | StreamProcessor (StreamM ())


data Node globalState = Node
  { nodeType  :: NodeType globalState
  , inputRefs :: [Unique]
  , outputRef :: Unique
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

newtype EvalASTM s a = EvalASTM { runEvalASTM :: RWS
                                                   ()
                                                   (Mutator L.Expression)
                                                   ( FunctionDict s
                                                   , UDict
                                                   , NameGenerator
                                                   , NameGenerator
                                                   , Unique
                                                   )
                                                   a }
  deriving (Functor, Applicative, Monad, MonadWriter (Mutator L.Expression))

class FreshUnique m where
  freshUnique :: m Unique

instance FreshUnique (EvalASTM s) where
  freshUnique = EvalASTM $ (_5 %= succ) >> use _5

tellMut :: MonadWriter (Mutator a) m => (a -> a) -> m ()
tellMut = tell . Mutator

defaultFunctionDict :: FunctionDict s
defaultFunctionDict = mempty -- TODO add smap and friends

evalASTM :: EvalASTM s a -> (FunctionDict s, L.Expression -> L.Expression, a)
evalASTM (EvalASTM ac) = (d, m, a)
  where
    (a, (d, _, _, _, _), Mutator m) = runRWS ac () (defaultFunctionDict, mempty, initNameGen mempty, initNameGen mempty, Unique 0)

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

evaluateAST :: ASTM s (Var a) -> (FunctionDict s, L.Expression)
evaluateAST (ASTM m) = (dict, build e)
  where
    (dict, build, e) = evalASTM $ do
      v <- iterM go m
      L.Var . L.Local <$> varToBnd v
    go :: ASTAction s (EvalASTM s (Var a)) -> EvalASTM s (Var a)
    go (InvokeSf sf tag vars cont) = do
      n <- registerFunction (CallSf $ SfRef sf tag)
      (rv, rb) <- mkRegVar
      vars' <- mapM varToBnd vars
      tellMut $ L.Let (Direct rb) (foldl L.Apply (L.Var (L.Sf n Nothing)) (map (L.Var . L.Local) vars'))
      cont rv
    go (Smap
         (innerCont :: Var inputType -> ASTM globalState (Var returnType))
         (Var inputVar)
         cont) = do
      (innerContVar, innerContBnd) <- mkRegVar
      let ASTM inner = innerCont innerContVar
      (e, Mutator build) <- censor (const mempty) $ listen $ iterM go inner
      resBnd <- varToBnd e
      (smapResVar, smapResBnd) <- mkRegVar
      tellMut $ L.Let (Direct smapResBnd) (L.Lambda (Direct innerContBnd) $ build (L.Var (L.Local resBnd)))
      cont smapResVar
      where
        iproxy = Proxy :: Proxy inputType

graphToAlgo :: FunctionDict s -> G.OutGraph -> Algorithm s a -- yeah ... have to figure out where `a` comes from ... not sure yet how
graphToAlgo dict G.OutGraph{..} = Algorithm (map (uncurry buildOp) opWRet) undefined -- I dont know where i should find this var right now ...
  where
    buildOp G.Operator{..} = Node (dict ! operatorType) (argDict ! operatorId)
    opWRet = zip operators [Unique 0..]
    retDict = Map.fromList $ map (first G.operatorId) opWRet
    -- The `sortOn` here is dangerous. it relies on there being *exactly one* input present for every
    -- argument to the function. If that invariant is broken we will not detect it here but get runtime errors
    argDict = fmap (map snd . sortOn fst) $ Map.fromListWith (++) $ map arcToUnique arcs
    arcToUnique G.Arc{G.target=G.Target{G.operator}, source=src}
      = ( operator
        , case src of
            G.LocalSource G.Target{..} -> pure (index, retDict ! operator)
            _                          -> error "invariant broken"
        )

runCompiler :: L.Expression -> IO G.OutGraph
runCompiler = undefined

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
newtype StreamM a = StreamM { runStreamM :: ExceptT (Maybe ()) (ReaderT ([Source Dynamic], [Sink Dynamic]) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Create a stream with a end that can only be sent to and one that can only be read from.
createStream :: IO (Sink a, Source a)
createStream = do
  chan <- newChan
  pure
    ( Sink $ writeChan chan
    , Source $ readChan chan
    )

-- | Send a packet to all output streams
sendPacket :: Packet Dynamic -> StreamM ()
sendPacket p = StreamM $ liftIO . mapM_ (($ p) . unSink) =<< asks snd

-- | Stop processing immediately. No subsequent actions are performed.
abortProcessing :: StreamM a
abortProcessing = StreamM (throwError Nothing)

-- | This can be used to gracefully end processing after an 'EndOfStreamPacket'
endProcessing :: Int -> StreamM a
endProcessing i = do
  numChans <- StreamM $ asks (length . fst)
  vals <- mapM (recievePacket <=< getSource) [x | x <- [0..numChans - 1], x /= i]
  sendEOS -- make sure the error of having excess input does not propagate unnecessarily
  if all isEOS vals
    then abortProcessing
    else
      -- eventually we'll want to send some information here which port had data left over in it
      StreamM $ throwError (Just ())

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
send = sendPacket . UserPacket . toDyn


-- Built in operators


smapIn :: Typeable inputType => Proxy inputType -> StreamM ()
smapIn (_ :: Proxy inputType) = do
  val <- recieve 0
  mapM_ send (val :: [inputType])


collectOp :: Typeable outputType => Proxy outputType -> StreamM ()
collectOp (_ :: Proxy outputType) = do
  i <- recieve 0
  (send :: [outputType] -> StreamM () ) =<< sequence (replicate i $ recieve 1)


sizeOp :: Typeable inputType => Proxy inputType -> StreamM ()
sizeOp (_ :: Proxy inputType) = send . (length :: [inputType] -> Int) =<< recieve 0


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
        -> StreamM ()
runFunc (SfRef (Sf f) accessor) stTrack = do
  inVars <- recieveAllUntyped
  s <- liftIO $ readIORef stTrack
  (ret, newState) <- liftIO $ runStateT (runSfMonad (applyVars f inVars)) (s ^. accessor)
  atomicModifyIORef'_ stTrack (accessor .~ newState)
  send ret


-- Running the stream


mountStreamProcessor :: StreamM () -> [Source Dynamic] -> [Sink Dynamic] -> IO ()
mountStreamProcessor process inputs outputs = do
  result <- runReaderT (runExceptT $ runStreamM $ forever safeProc) (inputs, outputs)
  case result of
    Left Nothing  -> pure () -- EOS marker appeared, this is what *should* happen
    Left (Just _) -> error "There were packets left over when a processor exited"
    Right () -> error "impossible"
  where
    safeProc
      | null inputs = process >> sendEOS >> abortProcessing
      | otherwise = forever process


customAsync :: IO a -> IO (Async a)
customAsync = async


runAlgo :: Typeable a => Algorithm globalState a -> globalState -> IO a
runAlgo (Algorithm nodes retVar) st = do
  stateVar <- newIORef st
  let outputMap :: Map.Map Unique [Sink Dynamic]
      outputMap = Map.fromList $ zip (map outputRef nodes) (repeat [])
      f (m, l) (Node func inputs oUnique) = do
        (sinks, sources) <- unzip <$> mapM (const createStream) inputs
        let newMap = foldl (\m (u, chan) -> Map.update (Just . (chan:)) u m) m (zip inputs sinks)
        pure (newMap, (func, sources, oUnique):l)
  (outMap, funcs) <- foldM f (outputMap, []) nodes

  (retSink, Source retSource) <- createStream

  let finalMap = Map.update (Just . (retSink:)) retVar outMap

  bracket
    (mapM (customAsync . \(nt, inChans, retUnique :: Unique) ->
      let outChans = fromMaybe (error "return value not found") $ Map.lookup retUnique finalMap
          processor =
            case nt of
              CallSf sfRef              -> runFunc sfRef stateVar
              StreamProcessor processor -> processor
      in mountStreamProcessor processor inChans outChans)
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
  forM_ gr $ \(Node nt vars sfRet) ->
    let ntStr = case nt of
                    CallSf _          -> "Sf"
                    StreamProcessor _ -> "StreamProcessor"
    in
    putStrLn $ ntStr ++ " with inputs " ++ show (map uniqueToInt vars) ++ " returns " ++ show (uniqueToInt sfRet)
  putStrLn $ "last return is " ++ show (uniqueToInt ret)


-- Types for the example


data T = T
data A = A
data B = B deriving Show
data C = C


-- Stateful functions for the example


stag :: Accessor s ()
stag = lens (const ()) const

sf1 :: ASTM s (Var T)
sf1 = call (liftSf (sfm $ liftIO (putStrLn "Executing sf1") >> pure T)) stag

sf2 :: Var T -> ASTM s (Var [Int])
sf2 = call (liftSf (\T -> sfm $ liftIO (putStrLn "Executing sf2") >> return [0..20])) stag

sf3 :: Var T -> Var Int -> ASTM s (Var Int)
sf3 = call (liftSf (\T i -> sfm $ liftIO (putStrLn "Executing sf3") >> return (succ i))) stag

aggregate :: Var Int -> ASTM Int (Var Int)
aggregate = call (liftSf (\i -> sfm $ S.modify (+ i) >> S.get)) (lens id (const id))


algorithm :: IO (Algorithm Int [Int])
algorithm = createAlgo $ do
  var1 <- sf1
  var3 <- sf2 var1
  v <- smap aggregate var3
  pure v
