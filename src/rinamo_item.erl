-module(rinamo_item).

-export([put_dynamo_to_solr/1, get_dynamo_to_solr/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

put_dynamo_to_solr(DynamoPut) ->
    %% unimplemented: conditional puts, accounting, return item
    BucketName = proplists:get_value("TableName", DynamoPut),
    Item = proplists:get_value("Item", DynamoPut),
    {BucketName, Item}.

get_dynamo_to_solr(DynamoGet) ->
    %% unimplemented: consistent read, accounting
    BucketName = proplists:get_value("TableName", DynamoGet),
    Keys = proplists:get_value("Keys", DynamoGet),
    Attributes = proplists:get_value("Attributes", DynamoGet),
    
    {BucketName, Keys, Attributes}.

-ifdef(TEST).

put_dynamo_to_solr_test() ->
    ok.

get_dynamo_to_solr_test() ->
    ok.

-endif.


