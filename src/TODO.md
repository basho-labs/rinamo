HTTP endpoint

* Do testing once everything's connected

Complete Operation handling

* Batch Get, Batch Put... there are stubs in rinamo_codec.erl
* Add GlobalSecondayIndexes support (merge with LocalSecondaryIndexes)
* Connect all operations to Riak JSON

Handle Web Requests

* Test the endpoint.
* AWS DynamoDB queryes usually go to /? so not sure how to handle the /rinamo/? endpoint...

Complete integration with riak_json core

* rinamo_rj is envisioned to communcate with lower level riak json stuff in order to handle queries, table creation etc