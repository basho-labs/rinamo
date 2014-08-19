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

-module(rinamo_crdt_counter).

-export([client/0]).
-export([increment/4, decrement/4, destroy/3]).
-export([value/3]).

-import(rinamo_crdt, [read_and_modify/6, get/4, delete/4]).

client() ->
    {ok, C} = riak:local_client(),
    C.

-spec increment(any(), binary(), binary(), term()) -> ok | notfound.
increment(Client, Bucket, Key, Scalar) ->
    read_and_modify(Client, Bucket, Key, Scalar, increment, riak_dt_pncounter).

-spec decrement(any(), binary(), binary(), term()) -> ok | notfound.
decrement(Client, Bucket, Key, Scalar) ->
    read_and_modify(Client, Bucket, Key, Scalar, decrement, riak_dt_pncounter).

-spec value(any(), binary(), binary()) -> {value, term()} | notfound.
value(Client, Bucket, Key) ->
    case get(Client, Bucket, Key, riak_dt_pncounter) of
        {ok, RO} ->
            {{_, Counter}, Stats} = riak_kv_crdt:value(RO, riak_dt_pncounter),
            [ riak_kv_stat:update(S) || S <- Stats ],
            {value, Counter};
        _ -> notfound
    end.

destroy(Client, Bucket, Key) ->
    delete(Client, Bucket, Key, riak_dt_pncounter).
