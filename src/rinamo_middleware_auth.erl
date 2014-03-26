-module(rinamo_middleware_auth).

-behaviour(cowboy_middleware).

-export([execute/2]).

-include("rinamo.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


execute(Req, Env) ->
  {AuthToken, _} = cowboy_req:header(?AMZ_AUTH_HEADER, Req),

  case AuthToken of
    undefined ->
      ErrorMsg = rinamo_error:make(missing_authentication_token),
      cowboy_req:reply(ErrorMsg#error.http_code,
        [{<<"content-type">>, <<"application/json">>}],
        rinamo_error:format(ErrorMsg), Req),
      {halt, Req};
    _ ->

      AccessKey = tokenize_auth_header(binary_to_list(AuthToken)),
      NewEnv = [{handler_opts, {access_key, AccessKey}} | lists:keydelete(handler_opts, 1, Env)],

      {ok, Req, NewEnv}
    end.

tokenize_auth_header(HeaderValue) ->
  case HeaderValue of
    undefined -> undefined;
    _ ->
      [_, Credentials, _, _] = string:tokens(HeaderValue, " "),
      StartCharPos = string:str(Credentials, "="),
      EndCharPos = string:str(Credentials, "/"),
      UserKey = string:substr(Credentials, StartCharPos + 1, EndCharPos - (StartCharPos + 1)),
      list_to_binary(UserKey)
  end.


-ifdef(TEST).

auth_fixture() ->
  {_, Fixture} = file:read_file("../tests/fixtures/access_key.txt"),
  binary_to_list(Fixture).

tokenize_auth_header_test() ->
  Input = auth_fixture(),
  Actual = tokenize_auth_header(Input),
  Expected = <<"RANDY_ACCESS_KEY">>,
  ?assertEqual(Expected, Actual),
  ?assertEqual(undefined, tokenize_auth_header(undefined)).

-endif.
