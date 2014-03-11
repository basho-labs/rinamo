DynamoDB

API Reference:
http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/Welcome.html

The following operations implemented (somewhat anyway)
http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_CreateTable.html

- Conversion of a create table doc into a yz XML schema. All keys/local 2i are currently going to be indexed. Other attributes would have to be indexed in order to show in scan results.
- Needs to be connected to riak json

http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_GetItem.html

- Conversion of a Get Item to a Solr query implemented.
- Needs to be connected to riak json

http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_PutItem.html

- Conversion of a Put Item to a form easier to deal with.
- Needs to be connected to riak json

http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Query.html

- Conversion of a Query doc to a solr query
- Needs to be connected to riak json

Code Flow, roughly:
rinamo_wm_endpoint -> rinamo_api -> rinamo_codec -> rinamo_tables -> rinamo_codec -> rinamo_api -> rinamo_wm_endpoint
