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
{-# LANGUAGE LambdaCase, Rank2Types #-}


module StreamsBasedFreeMonad where


import           Control.Monad
import           Control.Monad.Except
-- import           Control.Monad.Par        as P
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
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Void
import           Debug.Trace
import Lens.Micro


-- Utils


-- | Coerce a dynamic to a value.
-- If the expected type is not the one inside the 'Dynamic' it throws an error showing both types.
forceDynamic :: forall a . Typeable a => Dynamic -> a
forceDynamic dyn
  | Just a <- fromDynamic dyn = a
  | otherwise =  error $ "Mismatching types. Expected " ++ show rep ++ " got " ++ show (dynTypeRep dyn)
  where rep = typeRep (Proxy :: Proxy a)


-- The free monad


-- | A unique thing
data Unique = Unique { uniqueToInt :: Int } deriving (Ord, Eq)


instance Enum Unique where
    fromEnum = uniqueToInt
    toEnum = Unique

-- | Create a new unique thing
freshUnique :: MonadState Unique m => m Unique
freshUnique = do
    modify succ
    S.get
    

-- | A type tagged tracker for where data flows inside the program
newtype Var t = Var { unwrapVar :: Unique }


-- | The monad for the stateful function to run in
newtype SFMonad sfState ret = SFMonad { runSFMonad :: StateT sfState IO ret }
    deriving (Typeable, Monad, Applicative, Functor, MonadIO, MonadState sfState)


-- | A stateful funtion nicely packaged with all its required capabilities
data Sf fnType
    = forall state returnType .
    ( ReturnType fnType ~ SFMonad state returnType
    , Typeable returnType
    , ApplyVars fnType
    ) => Sf fnType -- reference to the actual function


type Accessor s a = Lens' s a


-- | Things you can do in the AST Monad
data ASTAction globalState a
    = forall sfType returnType sfState.
        ( ReturnType sfType ~ SFMonad sfState returnType
        ) =>
        InvokeSf
          (Sf sfType)
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

liftSf ::
    ( ReturnType f ~ SFMonad state ret
    , Typeable ret
    , ApplyVars f
    )
    => f -> Sf f
liftSf f = Sf f


type family MapFnType (f :: Type -> Type) t where
  MapFnType f (a -> b) = f a -> MapFnType f b
  MapFnType _ b = b

type family SetReturnType f t where
  SetReturnType f (a -> b) = a -> SetReturnType f b
  SetReturnType f _ = f


call :: ( newFnType ~ SetReturnType (ASTM globalState (Var ret)) (MapFnType Var f)
        , ReturnType f ~ SFMonad state ret
        , Ret newFnType ~ ret
        , GlobalState newFnType ~ globalState
        , CollectVars newFnType
        ) => Sf f -> Accessor globalState state -> newFnType
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



-- Evaluating the free monad



-- | Another way of referencing a stateful function. This one also contains an acessor for the state
data SFRef globalState
    = forall sfType state returnType
    . (ReturnType sfType ~ SFMonad state returnType)
    => SFRef (Sf sfType) (Accessor globalState state)


-- | This governs how a node is embedded.
-- Either a stateful function, which gets wrapped, or a stream processor which is
-- more powerful and does its own dataflow processing.
data NodeType globalState
    = CallSf (SFRef globalState)
    | StreamProcessor (StreamM ())


data Node globalState = Node
  { nodeType  :: NodeType globalState
  , inputRefs :: [Unique]
  , outputRef :: Unique
  }

data Algorithm globalState a = Algorithm [Node globalState] Unique


-- | Evaluate the AST Monad and ceate a graph
createAlgo :: ASTM s (Var a) -> Algorithm s a
createAlgo (ASTM m) = Algorithm w retVar
  where
    (Var retVar, _, w) = runRWS (iterM go m) () (Unique 0)
    go :: ASTAction s (RWS () [Node s] Unique b) -> RWS () [Node s] Unique b
    go (InvokeSf sf tag vars cont) =
        continue cont $ Node (CallSf $ SFRef sf tag) (map unwrapVar vars)
    go (Smap (innerCont :: Var inputType -> ASTM globalState (Var returnType)) (Var inputVar) cont) = do
        innerContVar <- freshUnique
        sizeRet <- freshUnique
        let iproxy = Proxy :: Proxy inputType
        tell
            [ Node (StreamProcessor $ smapIn iproxy) [inputVar] innerContVar
            , Node (StreamProcessor $ sizeOp iproxy) [inputVar] sizeRet
            ]
        let ASTM inner = innerCont (Var innerContVar)
        v <- iterM go inner
        collReturn <- freshUnique
        -- tell $ pure $ Node Collect [unwrapVar v] collReturn
        -- cont (Var collReturn)
        continue cont (Node (StreamProcessor (collectOp (Proxy :: Proxy returnType))) [sizeRet, unwrapVar v])

    continue :: (Var t -> RWS () [Node s] Unique b)
             -> (Unique -> Node s)
             -> RWS () [Node s] Unique b
    continue cont f = do
        u <- freshUnique
        tell $ pure $ f u
        cont (Var u)


-- The stream backend


data Packet a
    = UserPacket a
    | EndOfStreamPacket


isEOS :: Packet a -> Bool
isEOS EndOfStreamPacket = True
isEOS _ = False

newtype Stream a = Stream { unStream :: Chan (Packet a) }

newtype StreamM a = StreamM { runStreamM :: ExceptT (Maybe ()) (ReaderT ([Stream Dynamic], [Stream Dynamic]) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

createStream :: IO (Stream a)
createStream = Stream <$> newChan

sendPacket :: Packet Dynamic -> StreamM ()
sendPacket p = StreamM $ liftIO . mapM_ ((`writeChan` p) . unStream) =<< asks snd

abortProcessing :: StreamM a
abortProcessing = StreamM (throwError Nothing)

endProcessing :: Int -> StreamM a
endProcessing i = do
  numChans <- StreamM $ asks (length . fst)
  vals <- mapM (recievePacket <=< getStream) [x | x <- [0..numChans], x /= i]
  sendEOS -- make sure the error of having excess input does not propagate unnecessarily
  if all isEOS vals then
    abortProcessing
  else
    StreamM $ throwError (Just ())

sendEOS :: StreamM ()
sendEOS = sendPacket EndOfStreamPacket

recievePacket :: Stream a -> StreamM (Packet a)
recievePacket = StreamM . liftIO . readChan . unStream

getStream :: Int -> StreamM (Stream Dynamic)
getStream i = StreamM $ asks $ (!! i) . fst

recieveUntyped :: Int -> StreamM Dynamic
recieveUntyped i =
  getStream i >>= recievePacket >>= \case
    EndOfStreamPacket -> endProcessing i
    UserPacket u      -> return u

recieveAllUntyped :: StreamM [Dynamic]
recieveAllUntyped = StreamM (asks (length . fst)) >>= mapM recieveUntyped . enumFromTo 0

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
sizeOp (_ :: Proxy inputType) = send . dynApp (toDyn (length :: [inputType] -> Int)) =<< recieveUntyped 0


-- Running stateful functions as a Stream processor


atomicModifyIORef'_ :: MonadIO m => IORef a -> (a -> a) -> m ()
atomicModifyIORef'_ ref f = liftIO $ atomicModifyIORef' ref ((, ()) . f)


class ApplyVars t where
    applyVars :: t -> [Dynamic] -> ReturnType t


instance (ApplyVars b, Typeable a) => ApplyVars (a -> b) where
    applyVars func (x:xs) = applyVars (func $ forceDynamic x) xs
    applyVars _ []        = error "too few arguments"


instance ApplyVars (SFMonad state retType) where
    applyVars res [] = res
    applyVars _ _    = error "Too many arguments"


runFunc :: (ReturnType f ~ SFMonad state r)
        => Sf f
        -> Accessor globalState state
        -> IORef globalState
        -> StreamM ()
runFunc (Sf (f :: f)) accessor stTrack = do
  inVars <- recieveAllUntyped
  s <- liftIO $ readIORef stTrack
  (ret, newState) <- liftIO $ runStateT (runSFMonad (applyVars f inVars)) (s ^. accessor)
  atomicModifyIORef'_ stTrack (accessor .~ newState)
  send ret


-- Running the stream


mountStreamProcessor :: StreamM () -> [Stream Dynamic] -> [Stream Dynamic] -> IO ()
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
      

runAlgo :: Typeable a => Algorithm globalState a -> globalState -> IO a
runAlgo (Algorithm nodes retVar) st = do
    stateVar <- newIORef st
    let outputMap = Map.fromList $ zip (map outputRef nodes) (repeat []) :: Map.Map Unique [Stream Dynamic]
        f (m, l) (Node func inputs oUnique) = do
            inChans <- mapM (const createStream) inputs
            let newMap = foldl (\m (u, chan) -> Map.update (Just . (chan:)) u m) m (zip inputs inChans)
            pure (newMap, (func, inChans, oUnique):l)
    (outMap, funcs) <- foldM f (outputMap, []) nodes

    retStream@(Stream retChan) <- createStream

    let finalMap = Map.update (Just . (retStream:)) retVar outMap

    bracket
        (mapM (async . \(nt, inChans, retUnique :: Unique) ->
            let outChans = fromMaybe (error "return value not found") $ Map.lookup retUnique finalMap
                processor =
                  case nt of
                    CallSf (SFRef sf accessor) -> runFunc sf accessor stateVar
                    StreamProcessor processor -> processor
            in mountStreamProcessor processor inChans outChans
            ) funcs)
        ( mapM_ cancel )
        $ \threads -> do
            mapM_ link threads
            UserPacket ret <- readChan retChan
            EndOfStreamPacket <- readChan retChan
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
sf1 = call (liftSf (liftIO (putStrLn "Executing sf1") >> pure T :: SFMonad () T)) stag

sf2 :: Var T -> ASTM s (Var [Int])
sf2 = call (liftSf (\T -> liftIO (putStrLn "Executing sf2") >> return [0..20] :: SFMonad () [Int])) stag

sf3 :: Var T -> Var Int -> ASTM s (Var Int)
sf3 = call (liftSf (\T i -> liftIO (putStrLn "Executing sf3") >> return (succ i) :: SFMonad () Int)) stag

aggregate :: Var Int -> ASTM Int (Var Int)
aggregate = call (liftSf (\i -> S.modify (+ i) >> S.get :: SFMonad Int Int)) (lens id (const id))


algorithm :: Algorithm Int [Int]
algorithm = createAlgo $ do
    var1 <- sf1
    var3 <- sf2 var1
    v <- smap aggregate var3
    pure v
