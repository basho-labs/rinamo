-module(rinamo_rj).

-export([create_table/6]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

create_table(Table, Fields, KeySchema, L2I, ProvisionedThroughput, RawSchema) ->

  %% TOOD:  define & implement table storage model options
  %%   : option 1, {TableName, SolrSchema, _RawSchema},
  %%       store RawSchema as bucket metadata
  %%       rj_yz:store_schema(BucketName, SolrSchema).
  %%   : option 2, TBD

  {_, TableName} = Table,
  io:format("~s~n", [mochijson2:encode(TableName)]),

  ok.

get_item({TableName, SolrQuery}) ->
  ok.
    
put_item({TableName, Item}) ->
  ok.

query({TableName, Query}) ->
  ok.

-ifdef(TEST).

create_table_test() ->
  Input = [{"TableName", "table_name"},
           {"Fields", [{"attr_name", "attr_type"}]},
           {"KeySchema", [{"attr_name", "key_type"}]},
           {"LocalSecondaryIndexes", [[{"2i_name", 
                                       [{"KeySchema", [{"attr_name", "key_type"}]},
                                        {"Projection", [{"NonKeyAttributes", ["attr_name"]},
                                                        {"ProjectionType", "projection_type"}]}]
                                   }]]},
           {"ProvisionedThroughput", [{"ReadCapacityUnits", 10},
                                      {"WriteCapacityUnits", 2}]}
          ],
[ Table, Fields, KeySchema, L2I,
    ProvisionedThroughput ] = Input,

Actual = rinamo_rj:create_table(Table, Fields, KeySchema, L2I, ProvisionedThroughput, '{"raw":"schema"}'),
Expected = ok,
?assertEqual(Expected, Actual).

-endif.