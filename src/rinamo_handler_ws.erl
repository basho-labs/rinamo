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

-module(rinamo_handler_ws).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

% Behaviour cowboy_http_handler
-export([
    init/3,
    handle/2,
    terminate/3
]).

% Behaviour cowboy_websocket_handler
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

init(_Type, _Req, _Opts) ->
    % divert cowboy to switch to WS connections
    {upgrade, protocol, cowboy_websocket}.

% obligatory; Should never get here.
handle(Req, State) ->
    lager:debug("Unexpected request: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [
        {'Content-Type', <<"text/html">>}
    ]),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

% called before the handshake occurs
% can override response and shutdown using
% cowboy_req:reply/{2,3,4} and {shutdown, Req}
% timeout is in miliseconds
websocket_init(_Type, Req, _Opts) ->
    lager:debug("New client"),
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, undefined, 60000, hibernate}.

% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
    lager:debug("Received: ~p", [Msg]),
    {reply,
        {text, << "Responding to ", Msg/binary >>},
        Req, State, hibernate
    };
websocket_handle(_Any, Req, State) ->
    % things other than text
    {ok, Req, State, hibernate}.

% handle erlang messages
websocket_info({log, Text}, Req, State) ->
    {reply, {text, Text}, Req, State, hibernate};
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
