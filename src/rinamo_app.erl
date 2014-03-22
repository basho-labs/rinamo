-module(rinamo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case rinamo_config:is_enabled() of
        true -> 
            start_cowboy(),
            rinamo_sup:start_link();
        _ -> 
            ok
    end.

stop(_State) ->
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
