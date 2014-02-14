%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.
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
%% -------------------------------------------------------------------

-module(rinamo_item).

-export([put_dynamo_to_solr/1, get_dynamo_to_solr/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

put_dynamo_to_solr(DynamoPut) ->
    %% unimplemented: conditional puts, accounting, return item
    BucketName = proplists:get_value("TableName", DynamoPut),
    Item = proplists:get_value("Item", DynamoPut),
    {BucketName, Item}.

get_dynamo_to_solr(DynamoGet) ->
    %% unimplemented: consistent read, accounting
    BucketName = proplists:get_value("TableName", DynamoGet),
    Keys = proplists:get_value("Keys", DynamoGet),
    Attributes = proplists:get_value("Attributes", DynamoGet),
    
    {BucketName, Keys, Attributes}.

-ifdef(TEST).

put_dynamo_to_solr_test() ->
    ok.

get_dynamo_to_solr_test() ->
    ok.

-endif.


