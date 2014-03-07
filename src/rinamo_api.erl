-module(rinamo_api).

-export([create_table/2, list_tables/2, describe_table/2]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

create_table(DynamoRequest, AWSContext) ->
  % Parse Request
  [ {_, Table}, {_, Fields}, {_, KeySchema}, {_, LSI},
    {_, ProvisionedThroughput}, {_, RawSchema} ] = rinamo_codec:decode_create_table(DynamoRequest),
   
  % Creation Time
  {MegaSecs, Secs, MicroSecs} = now(),
  CreationTime = (MegaSecs * 1000000 + Secs) + MicroSecs / 1000000,

  % Put things into Riak
  {LR, TR} = rinamo_rj:create_table(Table, Fields, KeySchema, LSI, ProvisionedThroughput, RawSchema, AWSContext),

  % handle DB failures
  % Response = case DB_Result of
  %   {error,all_nodes_down} ->
  %   _ -> % proceed as normal
  % end,

  % Enrich Response as needed
  Response = [{ <<"TableDescription">>, [
    {<<"TableName">>, Table},
    {<<"AttributeDefinitions">>, [Fields]},
    {<<"KeySchema">>, [KeySchema]},
    {<<"ProvisionedThroughput">>, ProvisionedThroughput},
    {<<"LocalSecondaryIndexes">>, [{}]},
    {<<"GlobalSecondaryIndexes">>, [{}]},
    {<<"TableSizeBytes">>, 0},
    {<<"TableStatus">>, <<"CREATING">>},
    {<<"CreationDateTime">>, CreationTime}
  ]}],

  % JSONify the Response
  rinamo_codec:encode_create_table_response(Response).

list_tables(DynamoRequest, AWSContext) ->
  Result = rinamo_rj:list_tables(AWSContext),
  Response = [{ <<"TableNames">>, Result }],
  jsx:encode(Response).

describe_table(DynamoRequest, AWSContext) ->
  [ {_, Table} ] = rinamo_codec:decode_describe_table(DynamoRequest),
  Result = rinamo_rj:load_table_def(Table, AWSContext),
  Response = [{ <<"Table">>, Result }],
  jsx:encode(Response).

-ifdef(TEST).

create_table_test() ->
  meck:new(yz_kv, [non_strict]),
  meck:expect(yz_kv, client, fun() -> ok end),
  meck:expect(yz_kv, get, fun(_, _, _) -> {error, notfound} end),
  meck:expect(yz_kv, put, fun(_, _, _, _, _) -> ok end),

  Input = <<"{\"AttributeDefinitions\": [{ \"AttributeName\":\"Id\",\"AttributeType\":\"N\"}], \"TableName\":\"ProductCatalog\", \"KeySchema\":[{\"AttributeName\":\"Id\",\"KeyType\":\"HASH\"}], \"ProvisionedThroughput\":{\"ReadCapacityUnits\":10,\"WriteCapacityUnits\":5}}">>,
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  Response = rinamo_api:create_table(jsx:decode(Input), AWSContext),
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
  ?assert(CreationDateTime > 0),
  
  meck:unload(yz_kv).

-endif.
