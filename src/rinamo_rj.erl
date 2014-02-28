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

  % io:format(Table),
  % io:format(mochijson2:encode(Fields)),
  % io:format(mochijson2:encode(KeySchema)),
  % io:format(mochijson2:encode(L2I)),
  % io:format(mochijson2:encode(ProvisionedThroughput)),

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
          ].

-endif.