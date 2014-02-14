HTTP endpoint

* Handle operations (CreateTable, GetItem, PutItem, etc...)

Complete Operation handling

* Batch Get, Batch Put... there are stubs in rinamo_codec.erl
* Add GlobalSecondayIndexes support (merge with LocalSecondaryIndexes)

Handle Web Requests

* _wm_endpoint.erl - implement WM endpoint for rinamo. Not sure how this maps to existing dirvers

Complete integration with riak_json core

* pass through solr queries