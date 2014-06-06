-module(rinamo_config).

-export([
    is_enabled/0,
    get_protocol/0,
    get_bind_address/0,
    get_num_acceptors/0,
    get_root_path/0,
    get_index_strategy/0,
    get_auth_strategy/0,
    get_keystone_token/0,
    get_keystone_baseurl/0,
    get_ssl_cacertfile/0,
    get_ssl_certfile/0,
    get_ssl_keyfile/0
]).

is_enabled() ->
    get_env(enabled).

get_protocol() ->
    proplists:get_value(protocol, get_env(network)).

get_bind_address() ->
    proplists:get_value(bind, get_env(network)).

get_num_acceptors() ->
    proplists:get_value(acceptors, get_env(network)).

get_root_path() ->
    proplists:get_value(rootpath, get_env(url)).

get_index_strategy() ->
    proplists:get_value(strategy, get_env(index)).

get_auth_strategy() ->
    proplists:get_value(strategy, get_env(auth)).

get_keystone_token() ->
    proplists:get_value(token, get_env(keystone)).

get_keystone_baseurl() ->
    proplists:get_value(baseurl, get_env(keystone)).

get_ssl_cacertfile() ->
    proplists:get_value(cacertfile, get_env(ssl)).

get_ssl_certfile() ->
    proplists:get_value(certfile, get_env(ssl)).

get_ssl_keyfile() ->
    proplists:get_value(keyfile, get_env(ssl)).

%% Internal
get_env(Key) ->
    {_, Value} = application:get_env(rinamo, Key),
    Value.
