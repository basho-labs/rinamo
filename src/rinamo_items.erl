-module(rinamo_items).

-export([put_item/3]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

put_item(Table, Item, AWSContext) ->
  KeySchema = get_keyschema(Table, AWSContext),
  Key = get_key(Item, KeySchema),


  [{<<"response">>, <<"OK">>}].

%% Internal

get_keyschema(Table, AWSContext) ->
  % TableDef = rinamo_tables:load_table_def(Table, AWSContext),
  ok.

get_key(Item, KeySchema) ->
  ok.

-ifdef(TEST).

put_item_test() ->
  Table = <<"TableName">>,
  AWSContext=#ctx{ user_key = <<"TEST_API_KEY">> },
  Item = [{<<"PageCount">>,{<<"N">>,<<"600">>}},
   {<<"InPublication">>,{<<"N">>,<<"1">>}},
   {<<"ISBN">>,{<<"S">>,<<"222-2222222222">>}},
   {<<"Dimensions">>,{<<"S">>,<<"8.5 x 11.0 x 0.8">>}},
   {<<"Price">>,{<<"N">>,<<"20">>}},
   {<<"ProductCategory">>,{<<"S">>,<<"Book">>}},
   {<<"Id">>,{<<"N">>,<<"102">>}},
   {<<"Authors">>,{<<"SS">>,[<<"Author1">>,<<"Author2">>]}},
   {<<"Title">>,{<<"S">>,<<"Book 102 Title">>}}],
  Response = put_item(Table, Item, AWSContext),
  ok.

-endif.


