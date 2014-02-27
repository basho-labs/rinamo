-module(rinamo_wm_ping).

-export([
    init/1,
    to_html/2
    ]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

to_html(ReqData, Ctx) ->
    {"Everything is Awesome!", ReqData, Ctx}.