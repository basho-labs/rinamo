-module(rinamo_config).

-export([
    is_enabled/0,
    get_protocol/0,
    get_bind_address/0,
    get_num_acceptors/0
]).

is_enabled() ->
    application:get_env(rinamo, enabled, true).

get_protocol() ->
    proplists:get_value(protocol, application:get_env(rinamo, network)).

get_bind_address() ->
    proplists:get_value(bind, application:get_env(rinamo, network)).

get_num_acceptors() ->
    proplists:get_value(acceptors, application:get_env(rinamo, network)).
