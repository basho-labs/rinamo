-module(rinamo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("rinamo.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case rinamo_config:is_enabled() of
        true ->
            start_cowboy(),
            configure_riak(),
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
        https -> fun cowboy:start_https/4;
        spdy -> fun cowboy:start_spdy/4
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
                rinamo_middleware_reqid,
                rinamo_middleware_auth,
                rinamo_middleware_metering,
                cowboy_handler
            ]}
        ]
    ).

get_routes() ->
    [{'_', [
        {<<"/">>, rinamo_handler_root, []},
        {<<"/ping">>, rinamo_handler_ping, []},
        {<<"/ws">>, rinamo_handler_ws, []}
    ]}].

configure_riak() ->
    riak_core_bucket_type:create(?RINAMO_COUNTER_TYPE, [{datatype, counter}]),
    riak_core_bucket_type:activate(?RINAMO_COUNTER_TYPE),
    riak_core_bucket_type:create(?RINAMO_SET_TYPE, [{datatype, set}]),
    riak_core_bucket_type:activate(?RINAMO_SET_TYPE).
