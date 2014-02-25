%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.
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
%% -------------------------------------------------------------------

-module(rinamo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case rj_http_config:is_enabled() of
        true -> add_routes();
        _ -> ok
    end,
    
    riak_json_http_sup:start_link().

stop(_State) ->
    %TODO: Unregister Routes
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

add_routes() ->
    [webmachine_router:add_route(R) || R <- routes()].

props() ->
    [].

routes() ->
    [
        {[rinamo_config:endpoint_prefix()], rinamo_wm_endpoint, props()}
    ].