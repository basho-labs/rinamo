-module(rinamo_config).

-export([
    is_enabled/0,
    get_protocol/0,
    get_bind_address/0,
    get_num_acceptors/0,
    get_index_strategy/0
]).

is_enabled() ->
    get_env(enabled).

get_protocol() ->
    proplists:get_value(protocol, get_env(network)).

get_bind_address() ->
    proplists:get_value(bind, get_env(network)).

get_num_acceptors() ->
    proplists:get_value(acceptors, get_env(network)).

get_index_strategy() ->
    proplists:get_value(strategy, get_env(index)).

%% Internal
get_env(Key) ->
    {_, Value} = application:get_env(rinamo, Key),
    Value.
