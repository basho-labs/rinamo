-module(rinamo_crdt).

-export([read_and_modify/6, get/4, delete/4]).

-include("rinamo.hrl").
-include("rinamo_kv_types.hrl").

read_and_modify(Client, Bucket, Key, Value, Op, Type) ->
    {RO, Ctx} = read(Client, Bucket, Key, Type),
    CrdtOp = #crdt_op{mod=Type, op={Op, Value}, ctx=Ctx},
    modify(Client, RO, CrdtOp).

get(Client, Bucket, Key, Type) ->
    {_, BucketType} = lists:keyfind(Type, 1, ?RINAMO_CRDT_MAP),
    Client:get({BucketType, Bucket}, Key, []).

delete(Client, Bucket, Key, Type) ->
    {_, BucketType} = lists:keyfind(Type, 1, ?RINAMO_CRDT_MAP),
    Client:delete({BucketType, Bucket}, Key, []).

% Internal
read(Client, Bucket, Key, Type) ->
    GetResult = get(Client, Bucket, Key, Type),
    case GetResult of
        {ok, RO} ->
            {{Ctx, _}, _} = riak_kv_crdt:value(RO, Type),
            {RO, Ctx};
        _ ->
            {_, BucketType} = lists:keyfind(Type, 1, ?RINAMO_CRDT_MAP),
            {riak_kv_crdt:new({BucketType, Bucket}, Key, Type), undefined}
    end.

modify(Client, RO, CrdtOp) ->
    Options = [{crdt_op, CrdtOp}, {retry_put_coordinator_failure, false}],
    Client:put(RO, Options).
