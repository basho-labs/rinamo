-module(rinamo_crdt_set).

-export([client/0]).
-export([add/4, remove/4]).
-export([value/3]).

-import(rinamo_crdt, [read_and_modify/6, get/4]).

client() ->
    {ok,C} = riak:local_client(),
    C.

-spec value(any(), binary(), binary()) -> {value, list()} | notfound.
value(Client, Bucket, Key) ->
    case get(Client, Bucket, Key, riak_dt_orswot) of
        {ok, RO} -> {value, riak_kv_crdt:set_value(RO)};
        _ -> notfound
    end.

-spec add(any(), binary(), binary(), term() | list(term())) -> ok | {error, no_type}.
add(Client, Bucket, Key, Object) when is_list(Object) ->
    read_and_modify(Client, Bucket, Key, Object, add_all, riak_dt_orswot);
add(Client, Bucket, Key, Object) ->
    read_and_modify(Client, Bucket, Key, Object, add, riak_dt_orswot).

-spec remove(any(), binary(), binary(), term() | list(term())) -> ok | {error, no_type}.
remove(Client, Bucket, Key, Object) when is_list(Object) ->
    read_and_modify(Client, Bucket, Key, Object, remove_all, riak_dt_orswot);
remove(Client, Bucket, Key, Object) ->
    read_and_modify(Client, Bucket, Key, Object, remove, riak_dt_orswot).
