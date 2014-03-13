-module(rinamo_tables).

-export([create_table/3, list_tables/1,
         load_table_def/2, delete_table/2]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

create_table(Table, RawSchema, AWSContext) ->
  lager:debug("RawSchema: ~p~n", [RawSchema]),

  UserKey = AWSContext#ctx.user_key,

  B = UserKey,
  List_K = <<"TableList">>,
  Table_K = Table,
  Table_V = jsx:encode(RawSchema),

  {_, List} = yz_kv:get(
    yz_kv:client(), B, List_K
  ),

  lager:debug("Prior List State: ~p~n", [List]),

  List_V = case List of
    {insufficient_vnodes, _, _, _} -> throw(insufficient_vnodes);
    notfound -> update_table_list([], Table);
    _ -> update_table_list(jsx:decode(List), Table)
  end,

  % update table list
  R0 = yz_kv:put(
    yz_kv:client(),
    B, List_K, List_V,
    "application/json"),

  % save off table def
  R1 = yz_kv:put(
    yz_kv:client(),
    B, Table_K, Table_V,
    "application/json"),

  lager:debug("Result: [~p, ~p]~n", [R0, R1]),

  {R0, R1}.

list_tables(AWSContext) ->
  UserKey = AWSContext#ctx.user_key,

  B = UserKey,
  List_K = <<"TableList">>,

  {_, List} = yz_kv:get(
    yz_kv:client(), B, List_K
  ),

  case List of
    {insufficient_vnodes, _, _, _} -> throw(insufficient_vnodes);
    notfound -> [];
    _ -> jsx:decode(List)
  end.

load_table_def(Table, AWSContext) ->
  UserKey = AWSContext#ctx.user_key,

  B = UserKey,
  Table_K = Table,

  {_, Table_V} = yz_kv:get(
    yz_kv:client(), B, Table_K
  ),

  case Table_V of
    {insufficient_vnodes, _, _, _} -> throw(insufficient_vnodes);
    notfound -> notfound;
    _ -> jsx:decode(Table_V)
  end.

delete_table(Table, AWSContext) ->
  TD = load_table_def(Table, AWSContext),

  UserKey = AWSContext#ctx.user_key,

  B = UserKey,
  Table_K = Table,

  % remove table def
  C = yz_kv:client(),
  R0 = C:delete(B, Table_K),

  % remove from table list
  List_K = <<"TableList">>,
  List_V = jsx:encode(list_tables(AWSContext) -- [Table]),
  R1 = yz_kv:put(
    yz_kv:client(),
    B, List_K, List_V,
    "application/json"),

  TD.


%% Internal

update_table_list(TableList, Table) ->
  jsx:encode(lists:usort(lists:append(TableList, [Table]))).

-ifdef(TEST).

create_table_test() ->
  meck:new(yz_kv, [non_strict]),
  meck:expect(yz_kv, client, fun() -> ok end),
  meck:expect(yz_kv, get, fun(_, _, _) -> {value, <<"[\"one\",\"two\",\"three\"]">>} end),
  meck:expect(yz_kv, put, fun(_, _, _, _, _) -> ok end),

  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  Table = <<"TableName">>,
  Fields = [{<<"AttributeName">>, <<"attr_name">>}, {<<"AttributeType">>, <<"attr_type">>}],
  KeySchema = [{<<"AttributeName">>, <<"attr_name">>}, {<<"KeyType">>, <<"key_type">>}],
  LSI = [{"2i_name", [{<<"KeySchema">>, [{<<"AttributeName">>, <<"lsi_attr_name">>},
                                         {<<"KeyType">>, <<"lsi_key_type">>}]},
                      {<<"Projection">>,[{<<"NonKeyAttributes">>, [<<"attr_name">>]},
                                         {<<"ProjectionType">>, <<"projection_type">>}]}]}],
  ProvisionedThroughput = [{<<"ReadCapacityUnits">>, 10}, {<<"WriteCapacityUnits">>, 2}],

  Actual = create_table(Table, '{"raw":"schema"}', AWSContext),
  Expected = {ok, ok},
  ?assertEqual(Expected, Actual),

  meck:unload(yz_kv).

list_tables_test() ->
  meck:new(yz_kv, [non_strict]),
  meck:expect(yz_kv, client, fun() -> ok end),
  meck:expect(yz_kv, get, fun(_, _, _) -> {value, <<"[\"one\",\"two\",\"three\"]">>} end),

  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },

  Actual = list_tables(AWSContext),
  Expected = [<<"one">>, <<"two">>, <<"three">>],
  ?assertEqual(Expected, Actual),

  meck:unload(yz_kv).

load_table_def_test() ->
  meck:new(yz_kv, [non_strict]),
  meck:expect(yz_kv, client, fun() -> ok end),
  meck:expect(yz_kv, get, fun(_, _, _) -> {value, <<"[\"Some_Table_Def_JSON_Here\"]">>} end),

  Table = <<"Some_Table">>,
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },

  Actual = load_table_def(Table, AWSContext),
  Expected = [<<"Some_Table_Def_JSON_Here">>],
  ?assertEqual(Expected, Actual),

  meck:unload(yz_kv).


-endif.