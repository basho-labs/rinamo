-module(rinamo_wm_endpoint).

-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    content_types_accepted/2,
    malformed_request/2,
    resource_exists/2,
    process_post/2,
    post_is_create/2,
    create_path/2,
    accept_json/2
    ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {table_name}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(_) ->
	{ok, #ctx{}}.

service_available(ReqData, Context) ->
	{
		rinamo_config:is_enabled(),
		ReqData,
		Context
	}.

allowed_methods(ReqData, Context) ->
	{['POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
	{[{"application/json", accept_json}, 
	  {"application/x-amz-json-1.0", accept_json}], ReqData, Context}.

%% TODO, validate request?
malformed_request(ReqData, Context) ->
	{false, ReqData, Context}.

resource_exists(ReqData, Context) ->
	{true, ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

process_post(ReqData, Context) ->
	{true, ReqData, Context}.

% TODO:  revisit, add tenancy & table_name / item_name
create_path(ReqData, Context) ->
    K = riak_core_util:unique_id_62(),
    {K,
     wrq:set_resp_header("Location",
                         string:join(["base", K], "/"),
                         ReqData),
     Context}.

%% Non-webmachine

accept_json(ReqData, Context) ->
	Target = list_to_binary(wrq:get_req_header("X-Amz-Target", ReqData)),
	{Op, _ApiVersion} = dynamo_op(Target),
	Operation = case Op of
		"CreateTable" -> {rinamo_api, create_table};
		"UpdateTable" -> {error, unimplemented};
		"DeleteTable" -> {error, unimplemented};
		"ListTables" -> {error, unimplemented};
		"DescribeTable" -> {error, unimplemented};
		"GetItem" -> {rinamo_api, get_item};
		"PutItem" -> {rinamo_api, put_iem};
		"UpdateItem" -> {error, unimplemented};
		"DeleteItem" -> {error, unimplemented};
		"Query" -> {rinamo_api, query};
		"Scan" -> {error, unimplemented};
		"BatchGetItem" -> {error, unimplemented};
		"BatchWriteItem" -> {error, unimplemented};
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

