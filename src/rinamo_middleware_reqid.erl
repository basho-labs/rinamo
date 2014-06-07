%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------

-module(rinamo_middleware_reqid).
-behaviour(cowboy_middleware).
-export([execute/2]).

-include("rinamo.hrl").

-define(AMZ_REQ_ID_HEADER,<<"x-amzn-requestid">>).


execute(Req, Env) ->
    TaggedReq = cowboy_req:set_resp_header(?AMZ_REQ_ID_HEADER, create_request_id(), Req),
    {ok, TaggedReq, Env}.

create_request_id() ->
    uuid:uuid_to_string(uuid:get_v5_compat(uuid:get_v1(uuid:new(self(), erlang)))).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

create_request_id_test() ->
    ?assert(is_list(create_request_id())),
    ?assert(length(create_request_id()) >= 0).

-endif.
