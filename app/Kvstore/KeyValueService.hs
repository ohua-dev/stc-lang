
module Kvstore.KeyValueService where

import KvStore.Gen.KeyValueStore_Iface
import Data.Lazy.Text
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map


type KVStore = Map Text     -- table
                   (Map Text  -- key
                        (Map Text Text)) -- fields

data KVSHandler = KVSHandler (IORef KVStore)

serializeFields :: Map Text Text -> Text
serializeFields fields = undefined -- FIXME there must exist something that does that in the Thrift code

read :: Text -> Text -> Maybe (Set.Set Text) -> StateT KVStore (IO KVResponse)
read table key (Just fields) = do
  kvs <- get
  case (lookup table kvs) of
    Nothing -> return $ KVResponse READ Nothing Nothing $ Just "no such table!"
    (Just valTable) -> case (lookup key valTable) of
                          Nothing -> return $ KVResponse READ Nothing Nothing $ Just "no such key in table!"
                          (Just fieldVals) -> KVResponse READ
                                                        (Just (Set.filter (flip Set.member fields) fieldVals))
                                                        Nothing
read table key Nothing = return $ KVResponse READ (Just Map.empty) Nothing

scan :: Text -> Text -> Int -> StateT KVStore (IO KVResponse)
scan table key recordCount = do
  kvs <- get
  case (lookup table kvs) of
    Nothing -> return $ KVResponse SCAN Nothing Nothing $ Just "no such key!"
    (Just valTable) -> do
                        let collected = collect recordCount $ Map.assocs valTable
                        return $ KVResponse Nothing (Just $ Map.fromList collected) Nothing
  where
    collect remaining [] = []
    collect remaining ((k,v):xs) = if remaining >= recordCount
                                   then -- did not gather anything yet
                                      if k == key
                                      then [(k, (serializeFields v))] ++ collect (remaining-1) xs
                                   else -- already gathering values
                                      if remaining > 0
                                      then [(k, (serializeFields v))] ++ collect (remaining-1) xs
                                      else []

update :: Text -> Text -> Maybe (Map.Map Text Text) -> StateT KVStore (IO KVResponse)
update table key Noting = update table key $ Just Map.empty
update table key (Just values) = do
   kvs <- get
   case (lookup table kvs) of
     Nothing -> return $ KVResponse UPDATE Nothing Nothing $ Just "no such table!"
     (Just valTable) -> do
       case (lookup key valTable) of
         Nothing -> return $ KVResponse UPDATE Nothing Nothing $ Just "no such key!"
         (Just fields) -> do
           let fields' =  Map.union values fields
           let valTable' = Map.insert key fields' valTable
           let kvs' = Map.insert table valTable' kvs
           put kvs'
           return $ KVResponse UPDATE (Just Map.empty) Nothing Nothing

insert :: Text -> Text -> Maybe (Map.Map Text Text) -> StateT KVStore (IO KVResponse)
insert table key Noting = insert table key $ Just Map.empty
insert table key (Just values) = do
   kvs <- get
   case (lookup table kvs) of
     Nothing -> return $ KVResponse READ Nothing Nothing $ Just "no such table!"
     (Just valTable) -> do
                          let valTable' = Map.insert key values valTable
                          let kvs' = Map.insert table valTable' kvs
                          put kvs'
                          return $ KVResponse INSERT (Just Map.empty) Nothing Nothing

delete :: Text -> Text -> StateT KVStore (IO KVResponse)
delete table key = do
  kvs <- get
  case (lookup table kvs) of -- probably something that should be done even before request processing
    Nothing -> undefined -- TODO fetch table from a different node and load it into the kvs
    (Just valTable) -> do
                         let valTable' = Map.delete key valTable
                         put $ Map.update (\_ -> valTable') key kvs
                         return $ KVResponse DELETE Nothing Nothing

refreshCache :: Text -> Map.Map Text Text -> StateT KVStore (IO ())
refreshCache = do
  if $ Map.notMember table kvs
  then Nothing -> undefined -- TODO fetch table from a different node and load it into the kvs
  else return ()

handleReq :: KVRequest -> StateT KVStore (IO KVResponse)
handleRequest (KVRequest op table key fields recordCount values) = do
  refreshCache table
  case op of
    READ -> read table key fields
    SCAN -> scan table key recordCount
    UPDATE -> update table key values
    INSERT -> insert table key values
    DELETE -> delete table key

instance KeyValueStore_Iface KVSHandler where
  requests :: a -> (Vector.Vector KVRequest) -> IO (Vector.Vector KVResponse)
  requests (KVSHandler cacheRef) reqs = do
    cache <- readIORef cacheRef
    let (ioActions,nCache) = flip runStateT cache $ mapM handleRequest reqs
    writeIORef cacheRef nCache
    return ioActions
