-module(rinamo_response).

-export([make/1, send/3]).

-include("rinamo.hrl").

make(Data) ->
    Json = jsx:encode(Data),
    Crc32 = erlang:crc32(Json),
    {ok, Json, Crc32}.

send(Status, ResponseBody, Req) ->
    {ok, Json, Crc32} = make(ResponseBody),
    ReqWcrc32 = cowboy_req:set_resp_header(?AMZ_CRC32_HEADER, integer_to_binary(Crc32), Req),
    cowboy_req:reply(Status, [{<<"content-type">>, <<"application/json">>}], Json, ReqWcrc32).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_test() ->
    ?assert(false).

-endif.
