-module(rinamo_middleware_op).
-behaviour(cowboy_middleware).

-export([execute/2]).

-include("rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

execute(Req, Env) ->
  {AmzOp, _} = cowboy_req:header(?AMZ_OP_HEADER, Req),

  case AmzOp of
    undefined ->
      ErrorMsg = rinamo_error:make(missing_operation_target),
      cowboy_req:reply(ErrorMsg#error.http_code,
        [{<<"content-type">>, <<"application/json">>}],
        rinamo_error:format(ErrorMsg), Req),
      {halt, Req};
    _ ->

      {AWS_Op, AWS_API_Version} = extract_operation(binary_to_list(AmzOp)),

      {_, {_, HandlerOpts}, PartialEnv} = lists:keytake(handler_opts, 1, Env),
      lager:debug("Op HandlerOpts: ~p~n", [HandlerOpts]),
      State = case HandlerOpts of
        [] ->
          #state{operation = AWS_Op, aws_api_version = AWS_API_Version};
        _ ->
          HandlerOpts#state{operation = AWS_Op, aws_api_version = AWS_API_Version}
      end,

      NewEnv = [{handler_opts, State} | PartialEnv],

      {ok, Req, NewEnv}
  end.

%% Internal
extract_operation(OpHeader) ->
  case OpHeader of
    undefined -> {undefined, undefined};
    _ ->
      case string:chr(OpHeader, $.) of
        0 -> {OpHeader, undefined};
        Pos -> {
        list_to_binary(string:substr(OpHeader, Pos+1)),
        list_to_binary(string:substr(OpHeader, 1, Pos-1))}
      end
  end.


-ifdef(TEST).

extract_operation_test() ->
  TargetHeader = "DynamoDB_20120810.ListTables",
  Actual = extract_operation(TargetHeader),
  Expected = {<<"ListTables">>, <<"DynamoDB_20120810">>},
  ?assertEqual(Expected, Actual),
  ?assertEqual({undefined, undefined}, extract_operation(undefined)).

-endif.
