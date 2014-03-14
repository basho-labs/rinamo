-module(rinamo_items).

-export([put_item/3]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

put_item(Table, Item, AWSContext) ->
  {KeyAttribute, KeyType} = get_keyschema(Table, AWSContext),
  {FieldType, KeyValue} = kvc:path(KeyAttribute, Item),

  % TODO: Create PutItem Data Model & Save

  lager:debug("KeyAttribute: ~p, KeyType: ~p, FieldType: ~p, KeyValue: ~p",
              [KeyAttribute, KeyType, FieldType, KeyValue]).

%% Internal

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
  Response = put_item(Table, Item, AWSContext),

  meck:unload([rinamo_tables]).

-endif.
