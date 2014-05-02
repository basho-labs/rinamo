-module(rinamo_idx_item_proxies).

-export([
    store/4,
    query/4]).

-include("rinamo.hrl").

store(PartitionNS, PartitionId, Value, Item) ->
    % TODO: Locate and store this item in the right segment
    % (for now, assume just 1 segment)
    SegmentId = 1,

    % add the item to the segment, ordered by Value (range key)
    SB = erlang:iolist_to_binary([<<"Rinamo">>, ?RINAMO_SEPARATOR, <<"Index">>]),
    SK = erlang:iolist_to_binary([
        PartitionNS, ?RINAMO_SEPARATOR,
        PartitionId, ?RINAMO_SEPARATOR,
        SegmentId]),
    SV = {Value, Item},
    _ = rinamo_set:add(rinamo_set:client(), SB, SK, SV),

    % store a pointer to this segment
    B = PartitionNS,
    K = erlang:iolist_to_binary([PartitionId, ?RINAMO_SEPARATOR, Value]),
    V = [{<<"segment">>, 1}],
    _ = rinamo_kv:put(rinamo_kv:client(), B, K, V, "application/json"),

    ok.

query(PartitionNS, PartitionId, Query, Conditions) ->
    {Attribute, Operands, Operator} = Query,

    lager:debug("Attr, Operands, Operator: [~p, ~p, ~p]~n", [Attribute, Operands, Operator]),
    lager:debug("Conditions: ~p~n", [Conditions]),

    % TODO: narrow segments that need to be pulled using:
    %   - Segment Count + Segment Offset + Query + Conditions
    SegmentsToPull = [1],

    ItemList = fetch_items(SegmentsToPull, {PartitionNS, PartitionId}, []),

    ItemList.


fetch_items([], _, Acc) ->
    lists:reverse(Acc);
fetch_items([SegmentId|Rest], Partition, Acc) ->
    {PartitionNS, PartitionId} = Partition,
    SB = erlang:iolist_to_binary([<<"Rinamo">>, ?RINAMO_SEPARATOR, <<"Index">>]),
    SK = erlang:iolist_to_binary([
        PartitionNS, ?RINAMO_SEPARATOR,
        PartitionId, ?RINAMO_SEPARATOR,
        SegmentId]),
    {value, Segment} = rinamo_set:value(rinamo_set:client(), SB, SK),

    file:write_file("/tmp/segment", io_lib:fwrite("~p~n", [Segment])),

    Converted = lists:foldl(fun(S_Item, S_Acc) ->
        {RangeKey, ItemAttrList} = S_Item,
        % May as well check the range key & item attr list while here
        [ItemAttrList | S_Acc]
    end, [], Segment),
    fetch_items(Rest, Partition, Converted ++ Acc).
