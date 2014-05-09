-module(rinamo_counter).

-export([client/0]).
-export([increment/4, decrement/4]).
-export([value/3]).

-include("rinamo.hrl").
-include("rinamo_kv_types.hrl").

client() ->
    {ok,C} = riak:local_client(),
    C.

-spec increment(any(), binary(), binary(), term()) -> ok | notfound.
increment(Client, Bucket, Key, Scalar) ->
    read_and_modify(Client, Bucket, Key, Scalar, increment).

-spec decrement(any(), binary(), binary(), term()) -> ok | notfound.
decrement(Client, Bucket, Key, Scalar) ->
    read_and_modify(Client, Bucket, Key, Scalar, decrement).

-spec value(any(), binary(), binary()) -> {value, term()} | notfound.
value(Client, Bucket, Key) ->
    case get(Client, Bucket, Key) of
        {ok, RO} -> {value, riak_kv_crdt:counter_value(RO)};
        _ -> notfound
    end.

%% Internal

read_and_modify(Client, Bucket, Key, Scalar, Op) ->
    {RO, Ctx} = read(Client, Bucket, Key),
    CrdtOp = #crdt_op{mod=riak_dt_pncounter, op={Op, Scalar}, ctx=Ctx},
    modify(Client, RO, CrdtOp).

get(Client, Bucket, Key) ->
    Client:get({?RINAMO_COUNTER_TYPE, Bucket}, Key, []).

read(Client, Bucket, Key) ->
    GetResult = get(Client, Bucket, Key),
    case get(GetResult) of
        {ok, RO} ->
            {{Ctx, _}, _} = riak_kv_crdt:value(RO, riak_dt_pncounter),
            {RO, Ctx};
        _ ->
            {riak_kv_crdt:new({?RINAMO_COUNTER_TYPE, Bucket}, Key, riak_dt_pncounter), undefined}
    end.

modify(Client, RO, CrdtOp) ->
    Options = [{crdt_op, CrdtOp}, {retry_put_coordinator_failure, false}],
    Client:put(RO, Options).
