
import Kvstore.Gen.KeyValueStore_Iface
import Kvstore.Gen.KeyValueStore
import Kvstore.KeyValueService

main = do
  cacheRef <- newIORef
  handler <- KVSHandler cacheRef
  print "Starting the server ..."
  runBasicServer handler KeyValueStore.process 9090
