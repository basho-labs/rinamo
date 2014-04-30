-module(rinamo_middleware_reqid).
-behaviour(cowboy_middleware).
-export([execute/2]).

-include("rinamo.hrl").

-define(AMZ_REQ_ID_HEADER,<<"x-amzn-requestid">>).


execute(Req, Env) ->
    TaggedReq = cowboy_req:set_resp_header(?AMZ_REQ_ID_HEADER, create_request_id(), Req),
    {ok, TaggedReq, Env}.

create_request_id() ->
    uuid:uuid_to_string(uuid:get_v5_compat(<<"rinamo to rule them all">>)).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

create_request_id_test() ->
    ?assert(create_request_id()),
    ?assert(byte_size(create_request_id()) >= 0).

-endif.