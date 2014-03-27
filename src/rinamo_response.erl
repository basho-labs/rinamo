-module(rinamo_response).

-export([make/1]).


make(Data) ->
    Json = jsx:encode(Data),
    Crc32 = erlang:crc32(Json),
    {ok, Json, Crc32}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_test() ->
    ?assert(false).

-endif.
