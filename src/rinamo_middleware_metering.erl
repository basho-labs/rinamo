-module(rinamo_middleware_metering).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    {ok, Req, Env}.