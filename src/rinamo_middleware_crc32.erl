-module(rinamo_middleware_crc32).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    {ok, Req, Env}.