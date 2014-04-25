-module(rinamo_idx_one_for_one).

-export([
    store/4,
    query/3]).

-include("rinamo.hrl").

store(PartitionNS, PartitionId, Value, Item) ->
    % track the Item's dispersion via a dv_orswot
    RB = erlang:iolist_to_binary([<<"Rinamo">>, ?RINAMO_SEPARATOR, <<"Index">>]),
    RK = erlang:iolist_to_binary([
        PartitionNS, ?RINAMO_SEPARATOR,
        PartitionId, ?RINAMO_SEPARATOR,
        <<"RefList">>]),
    RV = Value,
    _ = rinamo_set:add(rinamo_set:client(), RB, RK, RV),

    % store Item as a dispersed object in Riak
    % logic for a get has to consider this may not succeed
    B = PartitionNS,
    K = erlang:iolist_to_binary([PartitionId, ?RINAMO_SEPARATOR, Value]),
    V = Item,
    _ = rinamo_kv:put(rinamo_kv:client(), B, K, V, "application/json"),

    ok.

query(PartitionNS, PartitionId, Query) ->
    {Attribute, Operator, Value} = Query,
    <<"Some Result">>.

% called from rinamo_items:  UserKey
delete(I, Do, Not, Know, Yet) ->
    ok.
