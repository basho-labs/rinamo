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
    {ok, Req, _Opts}.

handle(Req, State) ->

    lager:debug("Handler State: ~p~n", [State]),

    OpFun = case State#state.operation of
        <<"CreateTable">> -> {rinamo_api, create_table};
        <<"UpdateTable">> -> {error, unimplemented};
        <<"DeleteTable">> -> {rinamo_api, delete_table};
        <<"ListTables">> -> {rinamo_api, list_tables};
        <<"DescribeTable">> -> {rinamo_api, describe_table};
        <<"GetItem">> -> {rinamo_api, get_item};
        <<"PutItem">> -> {rinamo_api, put_item};
        <<"UpdateItem">> -> {error, unimplemented};
        <<"DeleteItem">> -> {rinamo_api, delete_item};
        <<"Query">> -> {error, unimplemented};
        <<"Scan">> -> {error, unimplemented};
        <<"BatchGetItem">> -> {error, unimplemented};
        <<"BatchWriteItem">> -> {error, unimplemented};
        _ -> {error, unimplemented}
    end,

    lager:debug("OpFun: ~p~n", [OpFun]),

    {_, Body, _} = cowboy_req:body(Req),

    {_, Req2} = case OpFun of
      {error, unimplemented} -> rinamo_error:make(operation_not_implemented);
      {Module, Function} ->
        Result = (catch erlang:apply(Module, Function, [jsx:decode(Body), State])),
        lager:debug("Operation Result: ~p~n", [Result]),
        case Result of
          _ when is_atom(Result) ->
            ErrorMsg = rinamo_error:make(Result),
            response(ErrorMsg#error.http_code, rinamo_error:format(ErrorMsg), Req);
          _ ->
            response(200, jsx:encode(Result), Req)
        end
    end,

    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% Internal
response(Status, ResponseBody, Req) ->
  cowboy_req:reply(Status, [{<<"content-type">>, <<"application/json">>}], ResponseBody, Req).
