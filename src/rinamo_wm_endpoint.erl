-module(rinamo_wm_endpoint).

-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    allow_missing_post/2,
    content_types_accepted/2,
    malformed_request/2,
    process_post/2,
    post_is_create/2,
    create_path/2,
    accept_json/2
    ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(_) ->
  {ok, #ctx{}}.

service_available(ReqData, Context=#ctx{}) ->
  {
    rinamo_config:is_enabled(),
    ReqData,
    Context
  }.

allow_missing_post(ReqData, Context) ->
  {false, ReqData, Context}.

allowed_methods(ReqData, Context) ->
  {['POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
  {[{"application/json", accept_json}, 
    {"application/x-amz-json-1.0", accept_json}], ReqData, Context}.

malformed_request(ReqData, Context) ->
  {false, ReqData, Context}.

process_post(ReqData, Context) ->
  {true, ReqData, Context}.

post_is_create(ReqData, Context) ->
  {true, ReqData, Context}.

% AWS does not set a Location, but they use a unique request id
create_path(ReqData, Context) ->
  K = riak_core_util:unique_id_62(),
  {K, wrq:set_resp_header("x-amzn-RequestId", K, ReqData), Context}.

%% Non-webmachine

accept_json(ReqData, Context) ->
  AWSContext = Context#ctx {
    user_key = dynamo_user(wrq:get_req_header("Authorization", ReqData))
  },
  {Op, _ApiVersion} = dynamo_op(wrq:get_req_header("X-Amz-Target", ReqData)),
  Operation = case Op of
    "CreateTable" -> {rinamo_api, create_table};
    "UpdateTable" -> {error, unimplemented};
    "DeleteTable" -> {rinamo_api, delete_table};
    "ListTables" -> {rinamo_api, list_tables};
    "DescribeTable" -> {rinamo_api, describe_table};
    "GetItem" -> {error, unimplemented};
    "PutItem" -> {rinamo_api, put_item};
    "UpdateItem" -> {error, unimplemented};
    "DeleteItem" -> {error, unimplemented};
    "Query" -> {error, unimplemented};
    "Scan" -> {error, unimplemented};
    "BatchGetItem" -> {error, unimplemented};
    "BatchWriteItem" -> {error, unimplemented};
    _ -> {error, unimplemented}
  end,

  lager:debug("AWSContext: ~p~n", [AWSContext]),
  lager:debug("Op: ~p~n", [Op]),

  case AWSContext#ctx.user_key of
    undefined -> {{halt, 403}, wrq:set_resp_body(
      format_error_message("MissingAuthenticationToken",
                           "Request must contain a valid (registered) access key ID."), ReqData), AWSContext};
    _ ->
      case Operation of
        {error, unimplemented} -> {{halt, 501}, wrq:set_resp_body(
          format_error_message("InternalServerErrorException", "Operation Not Specified."), ReqData), AWSContext};
        {Module, Function} -> 
          Result = (catch erlang:apply(Module, Function, [jsx:decode(wrq:req_body(ReqData)), AWSContext])),
          lager:debug("Operation Result: ~p~n", [Result]),
          case Result of
            insufficient_vnodes -> {{halt, 503}, wrq:set_resp_body(
              format_error_message("InternalServerErrorException", "Insufficient VNodes Available."), ReqData), AWSContext};
            table_exists -> {{halt, 409}, wrq:set_resp_body(
              format_error_message("InternalServerErrorException", "Cannot create preexisting table."), ReqData), AWSContext};
            _ ->
              Response_Json = jsx:encode(Result),
              {true, wrq:set_resp_body(Response_Json, ReqData), AWSContext}
          end
      end
  end.

%% Internal

dynamo_user(AuthorizationHeader) ->
  case AuthorizationHeader of
    undefined -> undefined;
    _ ->
      [_, Credentials, _, _] = string:tokens(AuthorizationHeader, " "),
      StartCharPos = string:str(Credentials, "="),
      EndCharPos = string:str(Credentials, "/"),
      UserKey = string:substr(Credentials, StartCharPos + 1, EndCharPos - (StartCharPos + 1)),
      list_to_binary(UserKey)
  end.

dynamo_op(TargetHeader) ->
  case TargetHeader of
    undefined -> {undefined, undefined};
    _ ->
        case string:chr(TargetHeader, $.) of
        0 -> {TargetHeader, undefined};
        Pos -> {string:substr(TargetHeader, Pos+1), string:substr(TargetHeader, 1, Pos-1)}
      end
  end.

format_error_message(Type, Message) ->
  "{\"__type\":\"com.amazonaws.dynamodb.v20120810#" ++ Type ++ "\",\"Message\":\"" ++ Message ++ "\"}".


-ifdef(TEST).

auth_fixture() ->
  "AWS4-HMAC-SHA256 Credential=RANDY_ACCESS_KEY/20140224/us-east-1/dynamodb/aws4_request, SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target, Signature=81f71d83f35b3b2be9589f9ec0f5edca95b14d602f639b183e729d1fd1e3308c".

dynamo_user_test() ->
  Input = auth_fixture(),
  Actual = dynamo_user(Input),
  Expected = <<"RANDY_ACCESS_KEY">>,
  ?assertEqual(Expected, Actual),
  ?assertEqual(undefined, dynamo_user(undefined)).

dynamo_op_test() ->
  TargetHeader = "DynamoDB_20120810.ListTables",
  Actual = dynamo_op(TargetHeader),
  Expected = {"ListTables", "DynamoDB_20120810"},
  ?assertEqual(Expected, Actual),
  ?assertEqual({undefined, undefined}, dynamo_op(undefined)).

-endif.
