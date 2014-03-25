-module(rinamo_handler_ping).

-behaviour(cowboy_http_handler).

-export([
    init/3,
    handle/2,
    terminate/3
]).

init(_Type, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.