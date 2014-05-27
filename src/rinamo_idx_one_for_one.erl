-module(rinamo_idx_one_for_one).

-export([
    store/4,
    query/4]).

-import(rinamo_results, [filter/2, eval/3, eval/4]).

-include("rinamo.hrl").

store(PartitionNS, PartitionId, Value, Item) ->
    % track the Item's dispersion via a dv_orswot
    RB = erlang:iolist_to_binary([<<"Rinamo">>, ?RINAMO_SEPARATOR, <<"Index">>]),
    RK = erlang:iolist_to_binary([
        PartitionNS, ?RINAMO_SEPARATOR,
        PartitionId, ?RINAMO_SEPARATOR,
        <<"RefList">>]),
    RV = Value,
    _ = rinamo_crdt_set:add(rinamo_crdt_set:client(), RB, RK, RV),

    SB = PartitionNS,
    SK = erlang:iolist_to_binary([PartitionId, ?RINAMO_SEPARATOR, Value]),
    SV = {Value, Item},
    _ = rinamo_crdt_set:add(rinamo_crdt_set:client(), SB, SK, SV),

    ok.

query(PartitionNS, PartitionId, Query, Conditions) ->
    {Attribute, Operands, Operator} = Query,
    RB = erlang:iolist_to_binary([<<"Rinamo">>, ?RINAMO_SEPARATOR, <<"Index">>]),
    RK = erlang:iolist_to_binary([
        PartitionNS, ?RINAMO_SEPARATOR,
        PartitionId, ?RINAMO_SEPARATOR,
        <<"RefList">>]),
    % TODO: handle notfound case
    {value, RefList} = rinamo_crdt_set:value(rinamo_crdt_set:client(), RB, RK),

    lager:debug("Attr, Operands, Operator: [~p, ~p, ~p]~n", [Attribute, Operands, Operator]),
    lager:debug("Conditions: ~p~n", [Conditions]),

    PreFilteredList = pre_filter(Operands, Operator, RefList),

    AllItems = fetch_items(PartitionNS, PartitionId, PreFilteredList, []),

    % filter (using KeyConditions) and convert to expected aws output format.
    % in short, this needs to strip the range key (used for ordering) out of
    % each element and just pass back the item attribute list.
    lists:foldl(fun(S_Item, S_Acc) ->
        {_, ItemAttrList} = S_Item,
        case filter_item(ItemAttrList, Conditions) of
            true -> [ItemAttrList | S_Acc];
            false -> S_Acc
        end
    end, [], AllItems).

delete(TBD) ->
    ok.

filter_item(ItemAttrList, KeyConditions) ->
    length(filter([ItemAttrList], KeyConditions)) > 0.

%% Internal
pre_filter(Operands, Operator, List) ->
    case Operands of
        [{OpType, OpVal}] ->
            pre_filter(OpType, OpVal, Operator, List);
        [{BOpType, BOpVal}, {EOpType, EOpVal}] ->
            pre_filter(BOpType, BOpVal, EOpType, EOpVal, Operator, List);
        _ ->
            throw(validation_operand_count)
    end.

pre_filter(_, OpVal, Operator, List) ->
    lists:filter(fun(X) -> eval(X, OpVal, Operator) end, List).

pre_filter(_, BeginVal, _, EndVal, Operator, List) ->
    lists:filter(fun(X) -> eval(X, BeginVal, EndVal, Operator) end, List).

% TODO:  limit fetch to 1 MB; return LastEvaluatedKey
fetch_items(_, _, [], Acc) ->
    lists:reverse(Acc);
fetch_items(B, PartitionId, [Ref|Rest], Acc) ->
    K = erlang:iolist_to_binary([PartitionId, ?RINAMO_SEPARATOR, Ref]),
    case rinamo_crdt_set:value(rinamo_crdt_set:client(), B, K) of
        {value, Items} ->
            NewAcc = lists:flatten([Items | Acc]),
            fetch_items(B, PartitionId, Rest, NewAcc);
        notfound ->
            % either we are partitioned, or the index is dirty
            % (we can't tell which)
            fetch_items(B, PartitionId, Rest, Acc);
        _ ->
            % what else can go wrong?
            fetch_items(B, PartitionId, Rest, Acc)
    end.
