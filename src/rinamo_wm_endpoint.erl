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

-module(rinamo_wm_endpoint).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {table_name}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(_) ->
	{ok, #ctx{}}.

%% TODO, check configuration, riak/solr status?
service_available(ReqData, Context) ->
	{true, ReqData, Context}.

allowed_methods(ReqData, Context) ->
	{['POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
	{[{"application/json", accept_json}, 
	  {"application/x-amz-json-1.0", accept_json}], ReqData, Context}.

%% TODO, validate request?
malformed_request(ReqData, Context) ->
	{false, ReqData, Context}.

%% Non-webmachine

accept_json(ReqData, Context) ->
	{Op, _ApiVersion} = dynamo_op(wrq:get_req_header("X-Amz-Target")),
	Operation = case Op of
		"BatchGetItem" -> {error, unimplemented};
		"BatchWriteItem" -> {error, unimplemented};
		"CreateTable" -> {rinamo_api, create_table};
		"DeleteItem" -> {error, unimplemented};
		"DeleteTable" -> {error, unimplemented};
		"DescribeTable" -> {error, unimplemented};
		"GetItem" -> {rinamo_api, get_item};
		"ListTables" -> {error, unimplemented};
		"PutItem" -> {rinamo_api, put_iem};
		"Query" -> {rinamo_api, query};
		"Scan" -> {error, unimplemented};
		"UpdateItem" -> {error, unimplemented};
		"UpdateTable" -> {error, unimplemented};
		_ -> {error, unimplemented}
	end,

	case Operation of
		{error, unimplemented} -> {{halt, 501}, ReqData, Context};
		{Module, Function} -> 
			Result = erlang:apply(Module, Function, [wrq:req_body(ReqData)]),
			{true, wrq:set_resp_body(Result, ReqData), Context}
	end.

%% Internal

dynamo_op(TargetHeader) ->
	Header = binary:bin_to_list(TargetHeader),
	case string:chr(Header, $.) of
		0 -> {Header, undefined};
		Pos -> {string:substr(Header, Pos+1), string:substr(Header, 1, Pos-1)}
	end.

-ifdef(TEST).

dynamo_op_test() ->
	TargetHeader = <<"DynamoDB_20120810.ListTables">>,
	Actual = dynamo_op(TargetHeader),
	Expected = {"ListTables", "DynamoDB_20120810"},
	?assertEqual(Expected, Actual).

-endif.

