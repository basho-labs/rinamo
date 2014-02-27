-module(rinamo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case rinamo_config:is_enabled() of
        true -> add_routes();
        _ -> ok
    end,
    
    rinamo_sup:start_link().

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
        {[rinamo_config:endpoint_prefix()], rinamo_wm_endpoint, props()},
        {[rinamo_config:endpoint_prefix(), "ping"], rinamo_wm_ping, props()}
    ].