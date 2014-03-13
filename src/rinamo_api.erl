-module(rinamo_api).

-export([create_table/2, list_tables/2, describe_table/2,
         put_item/2]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% Table Operations %%%

create_table(DynamoRequest, AWSContext) ->
  [ {_, Table}, {_, Fields}, {_, KeySchema}, {_, LSI},
    {_, ProvisionedThroughput}, {_, RawSchema} ] = rinamo_codec:decode_create_table(DynamoRequest),
   
  {MegaSecs, Secs, MicroSecs} = now(),
  CreationTime = (MegaSecs * 1000000 + Secs) + MicroSecs / 1000000,

  case rinamo_tables:load_table_def(Table, AWSContext) of
    notfound -> 
      % LR = List Result, TR = Table Result
      {LR, TR} = rinamo_tables:create_table(Table, RawSchema, AWSContext),

      % TODO: handle KV failures
      % Response = case LR or TR of
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
      ]}];
    _ ->
      table_exists
  end.


list_tables(_, AWSContext) ->
  Result = rinamo_tables:list_tables(AWSContext),
  Response = [{ <<"TableNames">>, Result }].

describe_table(DynamoRequest, AWSContext) ->
  [ {_, Table} ] = rinamo_codec:decode_describe_table(DynamoRequest),
  Result = rinamo_tables:load_table_def(Table, AWSContext),
  Response = [{ <<"Table">>, Result }].

%%% Item Operations %%%

put_item(DynamoRequest, AWSContext) ->
  [ {_, Expected}, {_, Item}, {return_consumed_capacity, _},
    {return_item_collection_metrics, _}, {return_values, _},
    {tablename, TableName}] = rinamo_codec:decode_put_item(DynamoRequest),

  Response = case TableName of
    [] -> <<"{\"Epic\":\"Fail\"}">>;
    _ -> rinamo_items:put_item(TableName, Item, AWSContext)
  end.

-ifdef(TEST).

table_fixture() ->
  <<"{\"AttributeDefinitions\": [{ \"AttributeName\":\"Id\",\"AttributeType\":\"N\"}], \"TableName\":\"ProductCatalog\", \"KeySchema\":[{\"AttributeName\":\"Id\",\"KeyType\":\"HASH\"}], \"ProvisionedThroughput\":{\"ReadCapacityUnits\":10,\"WriteCapacityUnits\":5}}">>.

item_fixture() ->
  <<"{\"TableName\":\"ProductCatalog\",\"Item\":{\"PageCount\":{\"N\":\"600\"},\"InPublication\":{\"N\":\"1\"},\"ISBN\":{\"S\":\"222-2222222222\"},\"Dimensions\":{\"S\":\"8.5 x 11.0 x 0.8\"},\"Price\":{\"N\":\"20\"},\"ProductCategory\":{\"S\":\"Book\"},\"Id\":{\"N\":\"102\"},\"Authors\":{\"SS\":[\"Author1\",\"Author2\"]},\"Title\":{\"S\":\"Book 102 Title\"}}}">>.

create_table_test() ->
  meck:new(rinamo_tables, [non_strict, passthrough]),
  meck:expect(rinamo_tables, load_table_def, 2, notfound),
  meck:expect(rinamo_tables, create_table, 3, {ok, ok}),

  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  Actual = rinamo_api:create_table(jsx:decode(table_fixture()), AWSContext),
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
  
  meck:unload(rinamo_tables).

list_tables_test() ->
  meck:new(rinamo_tables, [non_strict, passthrough]),
  meck:expect(rinamo_tables, list_tables, 1, [<<"Table_1">>, <<"Table_2">>]),

  Input = <<"">>,
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  Actual = rinamo_api:list_tables(Input, AWSContext),
  io:format("Actual ~p", [Actual]),
  [{<<"TableNames">>, [R0, R1]}] = Actual,
  ?assertEqual(<<"Table_1">>, R0),
  ?assertEqual(<<"Table_2">>, R1),

  meck:unload(rinamo_tables).

describe_table_test() ->
  TableDef = jsx:decode(table_fixture()),
  meck:new(rinamo_tables, [non_strict, passthrough]),
  meck:expect(rinamo_tables, load_table_def, 2, TableDef),

  Input = <<"{\"TableName\":\"ProductCatalog\"}">>,
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  Actual = rinamo_api:describe_table(Input, AWSContext),
  io:format("Actual ~p", [Actual]),
  [{<<"Table">>, ResultDef}] = Actual,
  ?assertEqual(ResultDef, TableDef),

  meck:unload(rinamo_tables).

put_item_test() ->
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  PutItem = jsx:decode(item_fixture()),
  Actual = rinamo_api:put_item(PutItem, AWSContext),
  io:format("Actual ~p", [Actual]),
  ok. % TODO

put_item_empty_test() ->
  Input = <<"{\"Item\":{}}">>,
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  Actual = rinamo_api:put_item(jsx:decode(Input), AWSContext),
  io:format("Actual ~p", [Actual]),
  ok. % TODO


-endif.
