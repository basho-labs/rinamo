-module(rinamo_api).

-export([create_table/1, put_item/1, get_item/1, query/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

create_table(DynamoRequest) ->
  % Parse Request
  [ {_, Table}, {_, Fields}, {_, KeySchema}, {_, LSI},
    {_, ProvisionedThroughput}, {_, RawSchema} ] = rinamo_codec:decode_create_table(DynamoRequest),
   
  % Creation Time
  {MegaSecs, Secs, MicroSecs} = now(),
  CreationTime = (MegaSecs * 1000000 + Secs) + MicroSecs / 1000000,

  % Put things into Riak
  _ = rinamo_rj:create_table(Table, Fields, KeySchema, LSI, ProvisionedThroughput, RawSchema),

  % Enrich Response as needed
  Response = [{ <<"TableDescription">>, [
    {<<"TableName">>, Table},
    {<<"AttributeDefinitions">>, [Fields]},
    {<<"KeySchema">>, [KeySchema]},
    {<<"ProvisionedThroughput">>, ProvisionedThroughput},
    {<<"LocalSecondaryIndexes">>, [{}]},
    {<<"GlobalSecondaryIndexes">>, [{}]},
    {<<"TableSizeBytes">>, 0},
    {<<"TableStatus">>, <<"'CREATING'">>},
    {<<"CreationDateTime">>, CreationTime}
  ]}],

  % JSONify the Response
  rinamo_codec:encode_create_table_response(Response).

put_item(DynamoRequest) ->
  Request = rinamo_codec:decode_put_itme(DynamoRequest),
  Response = rinamo_rj:create_table(Request),
  rinamo_codec:encode_put_item_response(Response).

get_item(DynamoRequest) ->
  Request = rinamo_codec:decode_get_item(DynamoRequest),
  Response = rinamo_rj:cput_item(Request),
  rinamo_codec:encode_get_item_response(Response).

query(DynamoRequest) ->
  Request = rinamo_codec:decode_get_item(DynamoRequest),
  Response = rinamo_rj:query_item(Request),
  rinamo_codec:encode_query_response(Response).

-ifdef(TEST).

create_table_test() ->
  Input = <<"{\"AttributeDefinitions\": [{ \"AttributeName\":\"Id\",\"AttributeType\":\"N\"}], \"TableName\":\"ProductCatalog\", \"KeySchema\":[{\"AttributeName\":\"Id\",\"KeyType\":\"HASH\"}], \"ProvisionedThroughput\":{\"ReadCapacityUnits\":10,\"WriteCapacityUnits\":5}}">>,  
  Response = rinamo_api:create_table(jsx:decode(Input)),
  Actual = jsx:decode(Response),
  io:format("Actual: ~p", [Actual]),
  [{_, [{_,TableName}, {_, AttributeDefinitions}, {_, KeySchema},
     {_, ProvisionedThroughput}, {_, LSI}, {_, GSI},
     {_, TableSize}, {_, TableStatus}, {_, CreationDateTime}]}] = Actual,
  ?assertEqual(<<"ProductCatalog">>, TableName),
  ?assertEqual([[
    {<<"AttributeName">>,<<"Id">>},
    {<<"AttributeType">>,<<"N">>}
  ]], AttributeDefinitions),
  ?assertEqual([[
    {<<"AttributeName">>,<<"Id">>},
    {<<"KeyType">>,<<"HASH">>}
  ]], KeySchema),
  ?assertEqual([
    {<<"ReadCapacityUnits">>,10},
    {<<"WriteCapacityUnits">>,5}
  ], ProvisionedThroughput),
  ?assertEqual([{}], LSI),
  ?assertEqual([{}], GSI),
  ?assertEqual(0, TableSize),
  ?assert(CreationDateTime > 0).

-endif.