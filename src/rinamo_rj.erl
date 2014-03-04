-module(rinamo_rj).

-export([create_table/6]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

create_table(Table, Fields, KeySchema, LSI, ProvisionedThroughput, RawSchema) ->

  %% TOOD:  define & implement table storage model options
  %%   : option 1, {Table, SolrSchema, _RawSchema},
  %%       store RawSchema as bucket metadata
  %%       rj_yz:store_schema(BucketName, SolrSchema).
  %%   : option 2, TBD

  io:format("~s~n", [jsx:encode(Table)]),

  ok.

get_item({TableName, SolrQuery}) ->
  ok.
    
put_item({TableName, Item}) ->
  ok.

query({TableName, Query}) ->
  ok.

-ifdef(TEST).

create_table_test() ->
  Table = <<"TableName">>,
  Fields = [{<<"AttributeName">>, <<"attr_name">>}, {<<"AttributeType">>, <<"attr_type">>}],
  KeySchema = [{<<"AttributeName">>, <<"attr_name">>}, {<<"KeyType">>, <<"key_type">>}],
  LSI = [{"2i_name", [{<<"KeySchema">>, [{<<"AttributeName">>, <<"lsi_attr_name">>},
                                         {<<"KeyType">>, <<"lsi_key_type">>}]},
                      {<<"Projection">>,[{<<"NonKeyAttributes">>, [<<"attr_name">>]},
                                         {<<"ProjectionType">>, <<"projection_type">>}]}]}],
  ProvisionedThroughput = [{<<"ReadCapacityUnits">>, 10}, {<<"WriteCapacityUnits">>, 2}],


  Actual = rinamo_rj:create_table(Table, Fields, KeySchema, LSI, ProvisionedThroughput, '{"raw":"schema"}'),
  Expected = ok,
  ?assertEqual(Expected, Actual).

-endif.