-module(rinamo_config).

-export([
    is_enabled/0,
    get_protocol/0,
    get_bind_address/0,
    get_num_acceptors/0
]).

is_enabled() ->
    app_helper:get_env(rinamo, enabled, true).

get_protocol() ->
    proplists:get(protocol, app_helper:get_env(rinamo, network)).

get_bind_address() ->
    proplists:get(bind, app_helper:get_env(rinamo, network)).

get_num_acceptors() ->
    proplists:get(acceptors, app_helper:get_env(rinamo, network)).