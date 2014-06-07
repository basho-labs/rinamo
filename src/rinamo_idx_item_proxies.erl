%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------

-module(rinamo_idx_item_proxies).

-export([
    store/4,
    query/4]).

-import(rinamo_results, [filter/2]).


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
    _ = rinamo_crdt_set:add(rinamo_crdt_set:client(), SB, SK, SV),

    % store a pointer to this segment
    B = PartitionNS,
    K = erlang:iolist_to_binary([PartitionId, ?RINAMO_SEPARATOR, Value]),
    V = [{<<"segment">>, 1}],
    _ = rinamo_kv:put(rinamo_kv:client(), B, K, V, "application/json"),

    ok.

query(PartitionNS, PartitionId, Query, Conditions) ->
    % this strategy applies all the conditions at once
    KeyConditions = [Query] ++ Conditions,

    % TODO: narrow segments that need to be pulled using:
    %   - Segment Count + Segment Offset + Query + Conditions
    SegmentsToPull = [1],

    ItemList = fetch_items(SegmentsToPull, {PartitionNS, PartitionId}, KeyConditions, []),

    ItemList.


fetch_items([], _, _, Acc) ->
    lists:reverse(Acc);
fetch_items([SegmentId|Rest], Partition, KeyConditions, Acc) ->
    {PartitionNS, PartitionId} = Partition,
    SB = erlang:iolist_to_binary([<<"Rinamo">>, ?RINAMO_SEPARATOR, <<"Index">>]),
    SK = erlang:iolist_to_binary([
        PartitionNS, ?RINAMO_SEPARATOR,
        PartitionId, ?RINAMO_SEPARATOR,
        SegmentId]),
    case rinamo_crdt_set:value(rinamo_crdt_set:client(), SB, SK) of
        {value, Segment} ->
            lager:debug("Segment: ~p~n", [Segment]),
            lager:debug("KeyConditions: ~p~n", [KeyConditions]),

            % filter (using KeyConditions) and convert to expected aws output format.
            % in short, this needs to strip the range key (used for ordering) out of
            % each element and just pass back the item attribute list.
            Converted = lists:foldl(fun(S_Item, S_Acc) ->
                {_, ItemAttrList} = S_Item,
                case filter_item(ItemAttrList, KeyConditions) of
                    true -> [ItemAttrList | S_Acc];
                    false -> S_Acc
                end
            end, [], Segment),
            fetch_items(Rest, Partition, KeyConditions, Converted ++ Acc);
        notfound ->
            fetch_items(Rest, Partition, KeyConditions, Acc)
    end.


filter_item(ItemAttrList, KeyConditions) ->
    length(filter([ItemAttrList], KeyConditions)) > 0.
