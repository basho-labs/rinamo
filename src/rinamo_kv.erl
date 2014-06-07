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

-module(rinamo_kv).

-export([client/0]).
-export([get/3, put/5, delete/3]).

-spec client() -> any().
client() ->
    {ok,C} = riak:local_client(),
    C.

-spec get(any(), binary(), binary()) -> any().
get(Client, Bucket, Key) ->
    case Client:get(Bucket, Key) of
      {ok, O} ->
          {value, riak_object:get_value(O)};
      Other ->
          Other
    end.

-spec put(any(), binary(), binary(), binary(), string()) -> ok.
put(Client, Bucket, Key, Value, ContentType) ->
    O = riak_object:new(Bucket, Key, Value, ContentType),
    Client:put(O).

-spec delete(any(), binary(), binary()) -> ok.
delete(Client, Bucket, Key) ->
    Client:delete(Bucket, Key).
