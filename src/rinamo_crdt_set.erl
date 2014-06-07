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

-module(rinamo_crdt_set).

-export([client/0]).
-export([add/4, remove/4, destroy/3]).
-export([value/3]).

-import(rinamo_crdt, [read_and_modify/6, get/4, delete/4]).

client() ->
    {ok,C} = riak:local_client(),
    C.

-spec value(any(), binary(), binary()) -> {value, list()} | notfound.
value(Client, Bucket, Key) ->
    case get(Client, Bucket, Key, riak_dt_orswot) of
        {ok, RO} -> {value, riak_kv_crdt:set_value(RO)};
        _ -> notfound
    end.

-spec add(any(), binary(), binary(), term() | list(term())) -> ok | {error, no_type}.
add(Client, Bucket, Key, Object) when is_list(Object) ->
    read_and_modify(Client, Bucket, Key, Object, add_all, riak_dt_orswot);
add(Client, Bucket, Key, Object) ->
    read_and_modify(Client, Bucket, Key, Object, add, riak_dt_orswot).

-spec remove(any(), binary(), binary(), term() | list(term())) -> ok | {error, no_type}.
remove(Client, Bucket, Key, Object) when is_list(Object) ->
    read_and_modify(Client, Bucket, Key, Object, remove_all, riak_dt_orswot);
remove(Client, Bucket, Key, Object) ->
    read_and_modify(Client, Bucket, Key, Object, remove, riak_dt_orswot).

destroy(Client, Bucket, Key) ->
    delete(Client, Bucket, Key, riak_dt_orswot).
