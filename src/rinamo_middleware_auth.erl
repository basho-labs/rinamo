-module(rinamo_middleware_auth).
-behaviour(cowboy_middleware).

-include("rinamo.hrl").

-export([execute/2]).

execute(Req, Env) ->
    AuthToken = tokenize_auth_header(cowboy_req:header(?AMZ_AUTH_HEADER, Req)),
    
    case AuthToken of
        undefined -> 
            ErrorMsg = rinamo_error:make(missing_authentication_token),
            {halt, _Req2} = cowboy_req:reply(ErrorMsg#error.http_code, [
                                            {<<"content-type">>, <<"application/json">>}
                                            ], rinamo_error:format(ErrorMsg), Req);
        _ ->
            {ok, Req, Env}
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
  binary:bin_to_list(Fixture).

tokenize_auth_header_test() ->
  Input = auth_fixture(),
  Actual = dynamo_user(Input),
  Expected = <<"RANDY_ACCESS_KEY">>,
  ?assertEqual(Expected, Actual),
  ?assertEqual(undefined, dynamo_user(undefined)).

-endif.