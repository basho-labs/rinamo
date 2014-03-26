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
            % the following prevents riak from starting cowboy
            % (if uncommented)
            % application:ensure_all_started(rinamo),

            % TODO: should check to see that cowboy actually starts
            start_cowboy(),
            rinamo_sup:start_link();
        _ ->
            ok
    end.

stop(_State) ->
    cowboy:stop_listener(rinamo_listener).

%% ===================================================================
%% Internal functions
%% ===================================================================

start_cowboy() ->
    CowboyStartFun = case rinamo_config:get_protocol() of
        http -> fun cowboy:start_http/4;
        https -> fun cowboy:start_https/4
    end,

    {RawIp, Port} = rinamo_config:get_bind_address(),
    {ok, Ip} = inet_parse:address(RawIp),
    NumAcceptors = rinamo_config:get_num_acceptors(),
    Dispatch = cowboy_router:compile(get_routes()),

    CowboyStartFun(rinamo_listener, NumAcceptors,
        [{ip, Ip}, {port, Port}],
        [
            {env, [{dispatch, Dispatch}]},
            {middlewares, [
              cowboy_router,
              %%rinamo_middleware_reqid,
              rinamo_middleware_auth,
              rinamo_middleware_op,
              rinamo_middleware_metering,
              cowboy_handler
              %%rinamo_middleware_crc32
            ]}
        ]
    ).

get_routes() ->
    [{'_', [
        {<<"/ping">>, rinamo_handler_ping, []},
        {<<"/">>, rinamo_handler_root, []}
    ]}].
