-module(rinamo_api).

-export([create_table/1, put_item/1, get_item/1, query/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

create_table(DynamoRequest) ->
  [{_, Table}, {_, Fields}, {_, KeySchema}, {_, L2I},
   {_, ProvisionedThroughput}, {_, RawSchema}] = rinamo_codec:decode_create_table(mochijson2:decode(DynamoRequest)),
   
  Response = rinamo_rj:create_table(Table, Fields, KeySchema, L2I, ProvisionedThroughput, RawSchema),
  rinamo_codec:encode_create_table_response(Response).

put_item(DynamoRequest) ->
  Request = rinamo_codec:decode_put_itme(DynamoRequest),
  Response = rinamo_rj:create_table(Request),
  rinamo_codec:encode_put_item_response(Response).

get_item(DynamoRequest) ->
  Request = rinamo_codec:decode_get_item(DynamoRequest),
  Response = rinamo_rj:cput_item(Request),
  rinamo_codec:encode_get_item_response(Response).

query(DynamoRequest) ->
  Request = rinamo_codec:decode_get_item(DynamoRequest),
  Response = rinamo_rj:query_item(Request),
  rinamo_codec:encode_query_response(Response).

-ifdef(TEST).
-endif.