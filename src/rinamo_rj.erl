-module(rinamo_rj).

-export([create_table/7]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

create_table(Table, Fields, KeySchema, LSI, ProvisionedThroughput, RawSchema, AWSContext) ->
  lager:debug("RawSchema: ~p~n", [RawSchema]),

  % rj_yz:store_schema(BucketName, SolrSchema).

  UserKey = AWSContext#ctx.user_key,

  B = UserKey,
  List_K = <<"TableList">>,
  Table_K = erlang:iolist_to_binary([Table]),
  Table_V = jsx:encode(RawSchema),

  {_, List} = yz_kv:get(
    yz_kv:client(), B, List_K
  ),

  List_V = case List of
    notfound -> jsx:encode([Table]);
    _ -> update_list(jsx:decode(List), Table)
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

get_item({TableName, SolrQuery}) ->
  ok.
    
put_item({TableName, Item}) ->
  ok.

query({TableName, Query}) ->
  ok.

%% Internal

update_list(TableList, Table) ->
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

  Actual = rinamo_rj:create_table(Table, Fields, KeySchema, LSI, ProvisionedThroughput, '{"raw":"schema"}', AWSContext),
  Expected = {ok, ok},
  ?assertEqual(Expected, Actual),

  meck:unload(yz_kv).

-endif.