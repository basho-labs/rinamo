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

start_cowboy() ->
    CowboyStartFun = case rinamo_config:get_protocol() of
        http -> fun cowboy:start_http/3;
        https -> fun cowboy:start_https/3
    end,

    {Ip, Port} = rinamo_config:get_bind_address(),
    NumAcceptors = rinamo_config:get_num_acceptors(),
    Dispatch = cowboy_router:compile(get_routes()),

    CowboyStartFun(rinamo_listener, NumAcceptors,
        [{ip, Ip}, {port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ).

get_routes() ->
    [
        %% {URIHost, list({URIPath, Handler, Opts})}
        {'_', [{'/ping', rinamo_handler_ping, []}]},
        {'_', [{'/', rinamo_handler_root, []}]}
    ].
