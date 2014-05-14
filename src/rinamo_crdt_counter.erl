-module(rinamo_crdt_counter).

-export([client/0]).
-export([increment/4, decrement/4, destroy/3]).
-export([value/3]).

-import(rinamo_crdt, [read_and_modify/6, get/4, delete/4]).

-include("rinamo.hrl").
-include("rinamo_kv_types.hrl").

client() ->
    {ok,C} = riak:local_client(),
    C.

-spec increment(any(), binary(), binary(), term()) -> ok | notfound.
increment(Client, Bucket, Key, Scalar) ->
    read_and_modify(Client, Bucket, Key, Scalar, increment, riak_dt_pncounter).

-spec decrement(any(), binary(), binary(), term()) -> ok | notfound.
decrement(Client, Bucket, Key, Scalar) ->
    read_and_modify(Client, Bucket, Key, Scalar, decrement, riak_dt_pncounter).

-spec value(any(), binary(), binary()) -> {value, term()} | notfound.
value(Client, Bucket, Key) ->
    case get(Client, Bucket, Key, riak_dt_pncounter) of
        {ok, RO} -> {value, riak_kv_crdt:counter_value(RO)};
        _ -> notfound
    end.

destroy(Client, Bucket, Key) ->
    delete(Client, Bucket, Key, riak_dt_pncounter).
