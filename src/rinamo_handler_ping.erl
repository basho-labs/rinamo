-module(rinamo_handler_ping).

-export([
    init/3,
    allowed_methods/2,
    content_types_provided/2
]).

init(_Type, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, ping_to_text}
    ], Req, State}.

ping_to_text(Req, State) ->
    {<<"OK">>, Req, State}.