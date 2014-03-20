-module(rinamo_items).

-export([put_item/3, get_item/3, delete_item/3]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec put_item(binary(), any(), #ctx{ user_key :: binary() }) -> ok.
put_item(Table, Item, AWSContext) ->
  UserKey = AWSContext#ctx.user_key,
  {KeyAttribute, KeyType} = get_keyschema(Table, AWSContext),
  [{FieldType, KeyValue}] = kvc:path(KeyAttribute, Item),

  lager:debug("KeyAttribute: ~p, KeyType: ~p, FieldType: ~p, KeyValue: ~p",
              [KeyAttribute, KeyType, FieldType, KeyValue]),

  case KeyType of
    <<"HASH">> -> store_hash_key(UserKey, Table, KeyValue, Item);
    <<"RANGE">> -> ok
  end.

-spec get_item(binary(), binary(), #ctx{ user_key :: binary() }) -> any().
get_item(Table, Key, AWSContext) ->
  UserKey = AWSContext#ctx.user_key,
  B = erlang:iolist_to_binary([UserKey, ?RINAMO_SEPARATOR, Table]),

  {_, Item_V} = rinamo_kv:get(
    rinamo_kv:client(), B, Key
  ),

  case Item_V of
    {insufficient_vnodes, _, _, _} -> throw(insufficient_vnodes);
    notfound -> notfound;
    _ -> jsx:decode(Item_V)
  end.

-spec delete_item(binary(), binary(), #ctx{ user_key :: binary() }) -> ok.
delete_item(Table, Key, AWSContext) ->
  UserKey = AWSContext#ctx.user_key,
  B = erlang:iolist_to_binary([UserKey, ?RINAMO_SEPARATOR, Table]),

  _ = rinamo_kv:delete(
    rinamo_kv:client(), B, Key
  ),

  ok.

%% Internal

-spec store_hash_key(binary(), binary(), [binary()], binary()) -> ok.
store_hash_key(User, Table, [], Item) ->
  ok;
store_hash_key(User, Table, [Key|Rest], Item) ->
  store_hash_key(User, Table, Rest, Item),
  store_hash_key(User, Table, Key, Item);
store_hash_key(User, Table, Key, Item) ->
  lager:debug("Storing: ~p:", [Key]),

  B = erlang:iolist_to_binary([User, ?RINAMO_SEPARATOR, Table]),
  Value = jsx:encode(Item),

  _ = rinamo_kv:put(
  rinamo_kv:client(),
  B, Key, Value,
  "application/json"),

  ok.

-spec get_keyschema(binary(), #ctx{ user_key :: binary() }) -> tuple().
get_keyschema(Table, AWSContext) ->
  TD = rinamo_tables:load_table_def(Table, AWSContext),
  case TD of
    notfound -> throw(table_missing);
    _ ->
      [AttributeName] = kvc:path("KeySchema.AttributeName", TD),
      [KeyType] = kvc:path("KeySchema.KeyType", TD),
      {AttributeName, KeyType}
  end.


-ifdef(TEST).

table_fixture() ->
  {_, Fixture} = file:read_file("../tests/fixtures/table.json"),
  Fixture.

item_fixture() ->
  {_, Fixture} = file:read_file("../tests/fixtures/item.json"),
  Fixture.

put_item_test() ->
  meck:new([rinamo_tables, rinamo_kv], [non_strict]),
  meck:expect(rinamo_tables, load_table_def, 2, jsx:decode(table_fixture())),
  meck:expect(rinamo_kv, client, 0, ok),
  meck:expect(rinamo_kv, put, 5, ok),

  Table = <<"TableName">>,
  Item = kvc:path("Item", jsx:decode(item_fixture())),
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  _ = put_item(Table, Item, AWSContext),

  meck:unload([rinamo_tables, rinamo_kv]).

get_item_test() ->
  meck:new(rinamo_kv, [non_strict]),
  meck:expect(rinamo_kv, client, 0, ok),
  meck:expect(rinamo_kv, get, 3, {value, <<"[\"Some_Item_Def_JSON_Here\"]">>}),

  Table = <<"Item Table">>,
  Key = <<"Some_Item_Key">>,
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },

  Actual = get_item(Table, Key, AWSContext),
  Expected = [<<"Some_Item_Def_JSON_Here">>],
  ?assertEqual(Expected, Actual),

  meck:unload(rinamo_kv).

delete_item_test() ->
  meck:new(rinamo_kv, [non_strict]),
  meck:expect(rinamo_kv, client, 0, ok),
  meck:expect(rinamo_kv, delete, 3, {value, <<"[\"Some_Item_Def_JSON_Here\"]">>}),

  Table = <<"Item Table">>,
  Key = <<"Some_Item_Key">>,
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },

  Actual = delete_item(Table, Key, AWSContext),
  Expected = ok,
  ?assertEqual(Expected, Actual),

  meck:unload(rinamo_kv).


-endif.
