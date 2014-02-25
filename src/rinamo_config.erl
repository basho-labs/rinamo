-module(rinamo_config).

-export([
          is_enabled/0,
          endpoint_prefix/0
    ]).

is_enabled() ->
    app_helper:get_env(rinamo, enabled, true).

endpoint_prefix() ->
    app_helper:get_env(rinamo, endpoint_prefix, "rinamo").
