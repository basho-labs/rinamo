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

  Bucket = erlang:iolist_to_binary([UserKey, <<"--">>, Table]),
  Key = <<"Randy_Key">>,
  Value = jsx:encode(RawSchema),

  Result = yz_kv:put(
    yz_kv:client(),
    Bucket,
    Key,
    Value,
    "application/json"),

  % TODO:  Handle failure: {error,all_nodes_down} vs ok.
  lager:debug("Result: ~p~n", [Result]),

  Result.

get_item({TableName, SolrQuery}) ->
  ok.
    
put_item({TableName, Item}) ->
  ok.

query({TableName, Query}) ->
  ok.

-ifdef(TEST).

create_table_test() ->
  meck:new(yz_kv, [non_strict]),
  meck:expect(yz_kv, client, fun() -> ok end),
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
  Expected = ok,
  ?assertEqual(Expected, Actual),

  meck:unload(yz_kv).

-endif.