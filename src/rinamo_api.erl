-module(rinamo_api).

-export([create_table/2, list_tables/2,
         describe_table/2, delete_table/2,
         put_item/2, get_item/2, delete_item/2]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% Table Operations %%%

% ---- Create Table ---- %

create_table(DynamoRequest, AWSContext) ->
  [ {_, Table}, {_, Fields}, {_, KeySchema}, {lsi, _},
    {_, ProvisionedThroughput}, {_, RawSchema} ] = rinamo_codec:decode_create_table(DynamoRequest),

  {MegaSecs, Secs, MicroSecs} = now(),
  CreationTime = (MegaSecs * 1000000 + Secs) + MicroSecs / 1000000,

  case rinamo_tables:load_table_def(Table, AWSContext) of
    notfound ->
      % TODO: We can make this async later, for now just drop the result.
      % {LR, TR) => (LR = List Result, TR = Table Result)
      {_, _} = rinamo_tables:create_table(Table, RawSchema, AWSContext),

      % Enrich Response as needed
      [{ <<"TableDescription">>, [
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

% ---- List Tables ---- %

list_tables(DynamoRequest, AWSContext) ->
  [ {_, ExclusiveStart},
    {_, Limit} ] = rinamo_codec:decode_list_tables(DynamoRequest),
  lager:debug("ExclusiveStart: ~p, Limit: ~p~n", [ExclusiveStart, Limit]),
  TableList = rinamo_tables:list_tables(AWSContext),
  { Result, AnyRemain } = filter_table_list(TableList, ExclusiveStart, Limit),
  case AnyRemain of
    true ->
      [{ <<"TableNames">>, Result },
       { <<"LastEvaluatedTableName">>, lists:last(Result) }];
    false ->
      [{ <<"TableNames">>, Result }]
  end.

-spec filter_table_list([binary()], any(), integer()) -> {[binary()], boolean()}.
filter_table_list(TableList, [], Limit) ->
  filter_table_list(TableList, Limit);
filter_table_list(TableList, ExclusiveStart, Limit) when is_binary(ExclusiveStart) ->
  {_, [_ | Rest]} = lists:splitwith(fun(X) -> X /= ExclusiveStart end, TableList),
  filter_table_list(Rest, Limit).

-spec filter_table_list([binary()], integer()) -> {[binary()], boolean()}.
filter_table_list(TableList, Limit) when is_integer(Limit), Limit >= 0, Limit < length(TableList) ->
  { Filtered, _ } = lists:split(Limit, TableList),
  { Filtered, true };
filter_table_list(TableList, _) ->
  { TableList, false }.

% ---- Describe Table ---- %

describe_table(DynamoRequest, AWSContext) ->
  [ {_, Table} ] = rinamo_codec:decode_table_name(DynamoRequest),
  Result = rinamo_tables:load_table_def(Table, AWSContext),
  case Result of
    notfound -> throw(table_missing);
    _ -> [{ <<"Table">>, Result }]
  end.

% ---- Delete Table ---- %

delete_table(DynamoRequest, AWSContext) ->
  [ {_, Table} ] = rinamo_codec:decode_table_name(DynamoRequest),
  Result = rinamo_tables:delete_table(Table, AWSContext),
  [{ <<"TableDescription">>, Result }].

%%% Item Operations %%%

% ---- Put Item ---- %

put_item(DynamoRequest, AWSContext) ->
  [ {expected, _}, {_, Item}, {return_consumed_capacity, _},
    {return_item_collection_metrics, _}, {return_values, _},
    {_, TableName}] = rinamo_codec:decode_put_item(DynamoRequest),

  case TableName of
    [] -> table_missing;
    _ ->
      % TODO: We can make this async later, result can be dropped.
      _ = rinamo_items:put_item(TableName, Item, AWSContext),
      [{}]
  end.

% ---- Get Item ---- %

get_item(DynamoRequest, AWSContext) ->
  [{attributes_to_get, _}, {consistent_read, _},
   {_, Keys}, {return_consumed_capacity, _},
   {_, TableName}] = rinamo_codec:decode_item_request(DynamoRequest),

  % assume for now they pass in the right
  % KeyAttribute & KeyType ("Id", "N")
  [{_, {_, Key}}] = Keys,

  Item = rinamo_items:get_item(TableName, Key, AWSContext),
  case Item of
      notfound -> [{}];
      _ -> [{ <<"Item">>, Item }]
  end.

% ---- Delete Item ---- %

delete_item(DynamoRequest, AWSContext) ->
  [{attributes_to_get, _}, {consistent_read, _},
   {_, Keys}, {return_consumed_capacity, _},
   {_, TableName}] = rinamo_codec:decode_item_request(DynamoRequest),

  [{_, {_, Key}}] = Keys,

  rinamo_items:delete_item(TableName, Key, AWSContext),

  [{}].

-ifdef(TEST).

table_fixture() ->
  {_, Fixture} = file:read_file("../tests/fixtures/table.json"),
  Fixture.

item_fixture() ->
  {_, Fixture} = file:read_file("../tests/fixtures/item.json"),
  Fixture.

get_request_fixture() ->
  {_, Fixture} = file:read_file("../tests/fixtures/get_item_request.json"),
  Fixture.

create_table_test() ->
  meck:new(rinamo_tables, [non_strict, passthrough]),
  meck:expect(rinamo_tables, load_table_def, 2, notfound),
  meck:expect(rinamo_tables, create_table, 3, {ok, ok}),

  AWSContext=#state{ user_key = <<"TEST_API_KEY">> },
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
  ?assertEqual(<<"CREATING">>, TableStatus),

  meck:unload(rinamo_tables).

list_tables_with_exclusive_start_test() ->
  meck:new(rinamo_tables, [non_strict, passthrough]),
  meck:expect(rinamo_tables, list_tables, 1, [<<"Table_1">>, <<"Table_2">>,
                                              <<"Table_3">>, <<"Table_4">>]),

  Input = <<"{\"ExclusiveStartTableName\":\"Table_1\", \"Limit\":2}">>,
  AWSContext=#state{ user_key = <<"TEST_API_KEY">> },
  Actual = rinamo_api:list_tables(jsx:decode(Input), AWSContext),
  io:format("Actual ~p", [Actual]),
  [{<<"TableNames">>, [T2, T3]},
   {<<"LastEvaluatedTableName">>, T3}] = Actual,
  ?assertEqual(<<"Table_2">>, T2),
  ?assertEqual(<<"Table_3">>, T3),

  meck:unload(rinamo_tables).

list_tables_test() ->
  meck:new(rinamo_tables, [non_strict, passthrough]),
  meck:expect(rinamo_tables, list_tables, 1, [<<"Table_1">>, <<"Table_2">>,
                                              <<"Table_3">>, <<"Table_4">>]),

  Input = <<"{}">>,
  AWSContext=#state{ user_key = <<"TEST_API_KEY">> },
  Actual = rinamo_api:list_tables(jsx:decode(Input), AWSContext),
  io:format("Actual ~p", [Actual]),
  [{<<"TableNames">>, [T1, T2, T3, T4]}] = Actual,
  ?assertEqual(<<"Table_1">>, T1),
  ?assertEqual(<<"Table_2">>, T2),
  ?assertEqual(<<"Table_3">>, T3),
  ?assertEqual(<<"Table_4">>, T4),

  meck:unload(rinamo_tables).

describe_table_test() ->
  TableDef = jsx:decode(table_fixture()),
  meck:new(rinamo_tables, [non_strict, passthrough]),
  meck:expect(rinamo_tables, load_table_def, 2, TableDef),

  Input = <<"{\"TableName\":\"ProductCatalog\"}">>,
  AWSContext=#state{ user_key = <<"TEST_API_KEY">> },
  Actual = rinamo_api:describe_table(jsx:decode(Input), AWSContext),
  io:format("Actual ~p", [Actual]),
  [{<<"Table">>, ResultDef}] = Actual,
  ?assertEqual(ResultDef, TableDef),

  meck:unload(rinamo_tables).

put_item_test() ->
  TableDef = jsx:decode(table_fixture()),
  meck:new([rinamo_tables, rinamo_items], [non_strict, passthrough]),
  meck:expect(rinamo_tables, load_table_def, 2, TableDef),
  meck:expect(rinamo_items, put_item, 3, ok),

  AWSContext=#state{ user_key = <<"TEST_API_KEY">> },
  PutItemRequest = jsx:decode(item_fixture()),
  [_, PutItemRequestNoTable] = PutItemRequest,

  R1 = rinamo_api:put_item(PutItemRequestNoTable, AWSContext),
  io:format("Actual ~p", [R1]),
  ?assertEqual(table_missing, R1),

  R2 = rinamo_api:put_item(PutItemRequest, AWSContext),
  io:format("Actual ~p", [R2]),
  ?assertEqual([{}], R2),

  meck:unload([rinamo_tables, rinamo_items]).

get_item_test() ->
  ItemWithTable = jsx:decode(item_fixture()),
  Item = kvc:path("Item", ItemWithTable),

  meck:new(rinamo_items, [non_strict, passthrough]),
  meck:expect(rinamo_items, get_item, 3, Item),

  Expected = [{<<"Item">>, Item}],

  GetItemRequest = jsx:decode(get_request_fixture()),
  AWSContext=#state{ user_key = <<"TEST_API_KEY">> },

  R0 = rinamo_api:get_item(GetItemRequest, AWSContext),
  io:format("Actual ~p", [R0]),
  ?assertEqual(Expected, R0),

  meck:unload(rinamo_items).

delete_item_test() ->
  meck:new(rinamo_items, [non_strict, passthrough]),
  meck:expect(rinamo_items, delete_item, 3, ok),
  Expected = [{}],

  DeleteItemRequest = jsx:decode(get_request_fixture()),
  AWSContext=#state{ user_key = <<"TEST_API_KEY">> },

  R0 = rinamo_api:delete_item(DeleteItemRequest, AWSContext),
  io:format("Actual ~p", [R0]),
  ?assertEqual(Expected, R0),

  meck:unload(rinamo_items).


-endif.
