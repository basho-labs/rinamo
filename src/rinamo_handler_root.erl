-module(rinamo_handler_root).

-export([
    init/3,
    service_available/2,
    allowed_methods/2,
    allow_missing_post/2,
    content_types_accepted/2,
    content_types_provided/2,
    create_path/2,
    accept_json/2
]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(_Type, Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

service_available(Req, State) ->
    {rinamo_config:is_enabled(), Req, State}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {"application/json", accept_json},
        {"application/x-amz-json-1.0", accept_json}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {"application/json", to_json},
        {"application/x-amz-json-1.0", to_json}
    ], Req, State}.



% AWS does not set a Location, but they use a unique request id
create_path(Req, State) ->
  K = riak_core_util:unique_id_62(),
  {K, wrq:set_resp_header("x-amzn-RequestId", K, Req), State}.

%% Non-webmachine

accept_json(Req, State) ->
  AWSState = State#ctx {
    user_key = dynamo_user(wrq:get_req_header("Authorization", Req))
  },
  {Op, _ApiVersion} = dynamo_op(wrq:get_req_header("X-Amz-Target", Req)),
  Operation = case Op of
    "CreateTable" -> {rinamo_api, create_table};
    "UpdateTable" -> {error, unimplemented};
    "DeleteTable" -> {rinamo_api, delete_table};
    "ListTables" -> {rinamo_api, list_tables};
    "DescribeTable" -> {rinamo_api, describe_table};
    "GetItem" -> {rinamo_api, get_item};
    "PutItem" -> {rinamo_api, put_item};
    "UpdateItem" -> {error, unimplemented};
    "DeleteItem" -> {rinamo_api, delete_item};
    "Query" -> {error, unimplemented};
    "Scan" -> {error, unimplemented};
    "BatchGetItem" -> {error, unimplemented};
    "BatchWriteItem" -> {error, unimplemented};
    _ -> {error, unimplemented}
  end,

  %lager:debug("AWSState: ~p~n", [AWSState]),
  %lager:debug("Op: ~p~n", [Op]),

  case AWSState#ctx.user_key of
    undefined -> {{halt, 403}, wrq:set_resp_body(
      format_error_message("MissingAuthenticationToken",
                           "Request must contain a valid (registered) access key ID."), Req), AWSState};
    _ ->
      case Operation of
        {error, unimplemented} -> {{halt, 501}, wrq:set_resp_body(
          format_error_message("InternalServerErrorException", "Operation Not Specified."), Req), AWSState};
        {Module, Function} ->
          Result = (catch erlang:apply(Module, Function, [jsx:decode(wrq:req_body(Req)), AWSState])),
   %       lager:debug("Operation Result: ~p~n", [Result]),
          case Result of
            insufficient_vnodes -> {{halt, 503}, wrq:set_resp_body(
              format_error_message("InternalServerErrorException", "Insufficient VNodes Available."), Req), AWSState};
            table_exists -> {{halt, 409}, wrq:set_resp_body(
              format_error_message("ResourceInUseException", "Cannot create preexisting table."), Req), AWSState};
            table_missing -> {{halt, 412}, wrq:set_resp_body(
              format_error_message("ResourceNotFoundException", "Cannot do operations on a non-existent table."), Req), AWSState};
            _ ->
              Response_Json = jsx:encode(Result),
              {{halt, 200}, wrq:set_resp_body(Response_Json, Req), AWSState}
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
  {_, Fixture} = file:read_file("../tests/fixtures/access_key.txt"),
  binary:bin_to_list(Fixture).

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
