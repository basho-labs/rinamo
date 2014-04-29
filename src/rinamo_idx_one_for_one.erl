-module(rinamo_idx_one_for_one).

-export([
    store/4,
    query/4]).

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

query(PartitionNS, PartitionId, Query, Conditions) ->
    {Attribute, Operands, Operator} = Query,
    % lookup the orswot
    RB = erlang:iolist_to_binary([<<"Rinamo">>, ?RINAMO_SEPARATOR, <<"Index">>]),
    RK = erlang:iolist_to_binary([
        PartitionNS, ?RINAMO_SEPARATOR,
        PartitionId, ?RINAMO_SEPARATOR,
        <<"RefList">>]),
    {value, RefList} = rinamo_set:value(rinamo_set:client(), RB, RK),

    lager:debug("Attr, Operands, Operator: [~p, ~p, ~p]~n", [Attribute, Operands, Operator]),

    % apply what parts of the filter that we can before item fetch
    % EQ | LE | LT | GE | GT | BEGINS_WITH | BETWEEN

    % lager:debug("Narrowed RefList:"),

    ItemList = fetch_items(PartitionNS, PartitionId, RefList, []),

    % apply post filter conditions

    ItemList.

% TODO:  limit fetch to 1 MB; return LastEvaluatedKey
fetch_items(B, PartitionId, [], Acc) ->
    lists:reverse(Acc);
fetch_items(B, PartitionId, [Ref|Rest], Acc) ->
    K = erlang:iolist_to_binary([PartitionId, ?RINAMO_SEPARATOR, Ref]),
    {value, JSON} = rinamo_kv:get(rinamo_kv:client(), B, K),
    Item = jsx:decode(JSON),
    fetch_items(B, PartitionId, Rest, [Item | Acc]).


delete(I, Do, Not, Know, Yet) ->
    ok.
