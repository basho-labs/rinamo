-module(rinamo_items).

-export([put_item/3]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

put_item(Table, Item, AWSContext) ->
  KeySchema = get_keyschema(Table, AWSContext),
  Key = get_key(Item, KeySchema),
  ok.

%% Internal

get_keyschema(Table, AWSContext) ->
  TableDef = rinamo_tables:load_table_def(Table, AWSContext),
  ok.

get_key(Item, KeySchema) ->
  ok.

-ifdef(TEST).

put_item_test() ->
  ok.

-endif.


