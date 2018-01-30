
enum Operation {
  READ = 1,
  SCAN = 2,
  UPDATE = 3,
  INSERT = 4,
  DELETE = 5
}

struct KVRequest {
  1: Operation op,
  2: string table,
  3: string key,
  4: optional set<string> fields,
  5: optional i32 recordCount
  6: optional map<string,string> values,
}

struct KVResponse {
  1: Operation op,
  2: optional map<string,string> values,
  3: optional list<map<string,string>> scannedValues,
  4: optional string failure
}

service KeyValueStore {
  list<KVResponse> requests(1:list<KVRequest> reqs)
}
