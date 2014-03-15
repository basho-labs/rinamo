-module(rinamo_items).

-export([put_item/3]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec put_item(binary(), any(), #ctx{ user_key :: binary() }) -> ok.
put_item(Table, Item, AWSContext) ->
  UserKey = AWSContext#ctx.user_key,
  {KeyAttribute, KeyType} = get_keyschema(Table, AWSContext),
  {FieldType, KeyValue} = kvc:path(KeyAttribute, Item),

  lager:debug("KeyAttribute: ~p, KeyType: ~p, FieldType: ~p, KeyValue: ~p",
              [KeyAttribute, KeyType, FieldType, KeyValue]),

  case KeyType of
    <<"HASH">> -> store_hash_key(UserKey, Table, KeyValue, Item);
    <<"RANGE">> -> ok
  end.

%% Internal

-spec store_hash_key(binary(), binary(), [binary()], binary()) -> ok.
store_hash_key(User, Table, [], Value) ->
  ok;
store_hash_key(User, Table, [Key|Rest], Value) ->
  store_hash_key(User, Table, Rest, Value),
  store_hash_key(User, Table, Key, Value);
store_hash_key(User, Table, Key, Value) ->
  lager:debug("Storing: ~p:", [Key]),

  % TODO: Create PutItem Data Model & Save
  % Bucket:  UserKey + Table
  % Key: KeyValue if KeyType == <<"HASH">>
  % Value: Item (json blob it)

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
  meck:new(rinamo_tables, [non_strict]),
  meck:expect(rinamo_tables, load_table_def, 2, jsx:decode(table_fixture())),

  Table = <<"TableName">>,
  Item = kvc:path(item, rinamo_codec:decode_put_item(jsx:decode(item_fixture()))),
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  _ = put_item(Table, Item, AWSContext),

  meck:unload([rinamo_tables]).

-endif.
