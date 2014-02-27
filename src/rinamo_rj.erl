-module(rinamo_rj).

-export([create_table/1]).


create_table(_RawSchema) ->
	%% TOOD:  define & implement table storage model options
	%%   : option 1, {TableName, SolrSchema, _RawSchema},
	%%       store RawSchema as bucket metadata
    %%       rj_yz:store_schema(BucketName, SolrSchema).
    %%   : option 2, TBD
    ok.

get_item({TableName, SolrQuery}) ->
    ok.
    

put_item({TableName, Item}) ->
    ok.

query({TableName, Query}) ->
    ok.
