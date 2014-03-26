-module(rinamo_middleware_reqid).

-behaviour(cowboy_middleware).

-export([execute/2]).

-include("rinamo.hrl").

-define(AMZ_REQ_ID_HEADER,<<"x-amzn-requestid">>).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

execute(Req, Env) ->
    Req2 = cowboy_req:set_resp_header(?AMZ_REQ_ID_HEADER, create_request_id(), Req),
    {ok, Req2, Env}.

create_request_id() ->
    uuid:get_v5_compat().