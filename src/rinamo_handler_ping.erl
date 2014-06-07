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
