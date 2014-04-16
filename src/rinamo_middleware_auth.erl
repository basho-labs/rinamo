-module(rinamo_middleware_auth).
-behaviour(cowboy_middleware).

-export([execute/2]).

-include("rinamo.hrl").

-define(AMZ_AUTH_HEADER,<<"authorization">>).

execute(Req, Env) ->
    {AuthToken, _} = cowboy_req:header(?AMZ_AUTH_HEADER, Req),

    case AuthToken of
        undefined ->
            case cowboy_req:path(Req) of
                {<<"/ping">>, _} -> {ok, Req, Env};
                _ ->
                    ErrorMsg = rinamo_error:make(missing_authentication_token),
                    rinamo_response:send(ErrorMsg#error.http_code, rinamo_error:format(ErrorMsg), Req),
                    {halt, Req}
            end;
        _ ->
            AccessKey = tokenize_auth_header(binary_to_list(AuthToken)),

            {_, {_, HandlerOpts}, PartialEnv} = lists:keytake(handler_opts, 1, Env),
            lager:debug("Auth HandlerOpts: ~p~n", [HandlerOpts]),
            State = case HandlerOpts of
                [] -> #state{user_key = AccessKey};
                _ -> HandlerOpts#state{user_key = AccessKey}
            end,

            NewEnv = [{handler_opts, State} | PartialEnv],

            {ok, Req, NewEnv}
    end.

%% Internal
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
-include_lib("eunit/include/eunit.hrl").

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
