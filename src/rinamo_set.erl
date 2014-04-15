-module(rinamo_set).

-export([client/0]).
-export([add/4, remove/4]).
-export([value/3]).

-include_lib("rinamo/include/rinamo_kv_types.hrl").

client() ->
  {ok,C} = riak:local_client(),
  C.

-spec value(any(), binary(), binary()) -> notfound | {value, list()}.
value(Client, Bucket, Key) ->
    case get(Client, Bucket, Key) of
        {ok, RO} -> {value, riak_kv_crdt:set_value(RO)};
        _ -> notfound
    end.

-spec add(any(), binary(), binary(), term() | list(term())) -> ok | {error,no_type}.
add(Client, Bucket, Key, Object) when is_list(Object) ->
    read_and_modify(Client, Bucket, Key, Object, add_all);
add(Client, Bucket, Key, Object) ->
    read_and_modify(Client, Bucket, Key, Object, add).

-spec remove(any(), binary(), binary(), term() | list(term())) -> ok | {error,no_type}.
remove(Client, Bucket, Key, Object) when is_list(Object) ->
    read_and_modify(Client, Bucket, Key, Object, remove_all);
remove(Client, Bucket, Key, Object) ->
    read_and_modify(Client, Bucket, Key, Object, remove).

%% Internal

read_and_modify(Client, Bucket, Key, Object, Op) ->
    {RO, Ctx} = read(Client, Bucket, Key),
    CrdtOp = #crdt_op{mod=riak_dt_orswot, op={Op, Object}, ctx=Ctx},
    modify(Client, RO, CrdtOp).

%% todo:  get rid of this fun
get(Client, Bucket, Key) ->
    Client:get({<<"set_bucket_type">>, Bucket}, Key, []).

read(Client, Bucket, Key) ->
    GetResult = get(Client, Bucket, Key),
    case get(GetResult) of
        {ok, RO} ->
            % {{Ctx, _Vector_Clock}, _State}
            {{Ctx, _}, _} = riak_kv_crdt:value(RO, riak_dt_orswot),
            {RO, Ctx};
        _ ->
            {riak_kv_crdt:new({<<"set_bucket_type">>, Bucket}, Key, riak_dt_orswot), undefined}
    end.

modify(Client, RO, CrdtOp) ->
    Options = [{crdt_op, CrdtOp}, {retry_put_coordinator_failure,false}],
    Client:put(RO, Options).
