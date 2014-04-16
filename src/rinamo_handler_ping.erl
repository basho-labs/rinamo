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
    {ok, Version} = application:get_key(rinamo, vsn),
    {_, NewReq} = rinamo_response:send(200, [
        {<<"message">>, <<"pong">>},
        {<<"version">>, erlang:list_to_binary(Version)}], Req),
    {ok, NewReq, State}.

terminate(_Reason, _Req, _State) ->
    ok.
