-module(rinamo_handler_root).
-behaviour(cowboy_http_handler).

-export([
    init/3,
    handle/2,
    terminate/3
]).

-include_lib("rinamo/include/rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(_Type, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
%    {Op, _ApiVersion} = dynamo_op(cowboy_req:header(?AMZ_OP_HEADER, Req)),

%    Operation = case Op of
%        <<"CreateTable">> -> {rinamo_api, create_table};
%        <<"UpdateTable">> -> {error, unimplemented};
%        <<"DeleteTable">> -> {rinamo_api, delete_table};
%        <<"ListTables">> -> {rinamo_api, list_tables};
%        <<"DescribeTable">> -> {rinamo_api, describe_table};
%        <<"GetItem">> -> {rinamo_api, get_item};
%        <<"PutItem">> -> {rinamo_api, put_item};
%        <<"UpdateItem">> -> {error, unimplemented};
%        <<"DeleteItem">> -> {rinamo_api, delete_item};
%        <<"Query">> -> {error, unimplemented};
%        <<"Scan">> -> {error, unimplemented};
%        <<"BatchGetItem">> -> {error, unimplemented};
%        <<"BatchWriteItem">> -> {error, unimplemented};
%        _ -> {error, unimplemented}
%    end,

    {ok, Req2} = cowboy_req:reply(200, [
      {<<"content-type">>, <<"text/plain">>}
    ], <<"The Response Body">>, Req),

    {ok, Req2, State}.

%    lager:debug("AWSState: ~p~n", [AWSState]),
%    lager:debug("Op: ~p~n", [Op]),


    %% http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_req/index.html
    %% TODO: this section needs to be updated to follow the Cowboy request/response specs.
%    case Operation of
%        {error, unimplemented} -> {{halt, 501}, wrq:set_resp_body(
%            format_error_message("InternalServerErrorException", "Operation Not Specified."), Req), AWSState};
%        {Module, Function} ->
%            Result = (catch erlang:apply(Module, Function, [jsx:decode(wrq:req_body(Req)), AWSState])),
%            lager:debug("Operation Result: ~p~n", [Result]),
%            case Result of
%                insufficient_vnodes -> {{halt, 503}, wrq:set_resp_body(
%                    format_error_message("InternalServerErrorException", "Insufficient VNodes Available."), Req), AWSState};
%                table_exists -> {{halt, 409}, wrq:set_resp_body(
%                    format_error_message("ResourceInUseException", "Cannot create preexisting table."), Req), AWSState};
%                table_missing -> {{halt, 412}, wrq:set_resp_body(
%                    format_error_message("ResourceNotFoundException", "Cannot do operations on a non-existent table."), Req), AWSState};
%                _ ->
%                    Response_Json = jsx:encode(Result),
%                    {{halt, 200}, wrq:set_resp_body(Response_Json, Req), AWSState}
%            end
%    end.

terminate(_Reason, _Req, _State) ->
	ok.

%% Internal
create_request_id() ->
    uuid:get_v5_compat().


%% TODO: this needs to be updated to work with binaries instead of strings.
dynamo_op(OpHeader) ->
  case OpHeader of
    undefined -> {undefined, undefined};
    _ ->
      case string:chr(OpHeader, $.) of
        0 -> {OpHeader, undefined};
        Pos -> {string:substr(OpHeader, Pos+1), string:substr(OpHeader, 1, Pos-1)}
      end
  end.


-ifdef(TEST).

dynamo_op_test() ->
  TargetHeader = "DynamoDB_20120810.ListTables",
  Actual = dynamo_op(TargetHeader),
  Expected = {"ListTables", "DynamoDB_20120810"},
  ?assertEqual(Expected, Actual),
  ?assertEqual({undefined, undefined}, dynamo_op(undefined)).

-endif.
