-module(rinamo_idx_item_proxies).

-export([
    store/4,
    query/4]).

-include("rinamo.hrl").

store(PartitionNS, PartitionId, Value, Item) ->
    ok.

query(PartitionNS, PartitionId, Query, Conditions) ->
    [{}].
