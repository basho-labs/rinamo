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
