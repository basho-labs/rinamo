-module(rinamo_items).

-export([put_item/3]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec put_item(binary(), any(), #ctx{ user_key :: binary() }) -> ok.
put_item(Table, RawItem, AWSContext) ->
  UserKey = AWSContext#ctx.user_key,
  {KeyAttribute, KeyType} = get_keyschema(Table, AWSContext),
  [{FieldType, KeyValue}] = kvc:path(KeyAttribute, RawItem),

  lager:debug("KeyAttribute: ~p, KeyType: ~p, FieldType: ~p, KeyValue: ~p",
              [KeyAttribute, KeyType, FieldType, KeyValue]),

  case KeyType of
    <<"HASH">> -> store_hash_key(UserKey, Table, KeyValue, RawItem);
    <<"RANGE">> -> ok
  end.

%% Internal

-spec store_hash_key(binary(), binary(), [binary()], binary()) -> ok.
store_hash_key(User, Table, [], RawItem) ->
  ok;
store_hash_key(User, Table, [Key|Rest], RawItem) ->
  store_hash_key(User, Table, Rest, RawItem),
  store_hash_key(User, Table, Key, RawItem);
store_hash_key(User, Table, Key, RawItem) ->
  lager:debug("Storing: ~p:", [Key]),

  B = erlang:iolist_to_binary([User, ?RINAMO_SEPARATOR, Table]),
  Value = jsx:encode(RawItem),

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

-endif.
