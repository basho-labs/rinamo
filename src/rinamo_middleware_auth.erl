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
                {<<"/ws">>, _} -> {ok, Req, Env};
                _ ->
                    ErrorMsg = rinamo_error:make(missing_authentication_token),
                    {_, NextReq} = rinamo_response:send(ErrorMsg#error.http_code, rinamo_error:format(ErrorMsg), Req),
                    {halt, NextReq}
            end;
        _ ->
            {Method, _} = cowboy_req:method(Req),
            case Method of
                <<"POST">> ->
                    OwnerKey = execute_auth_handler(AuthToken, Req),
                    case OwnerKey of
                        none ->
                            ErrorMsg = rinamo_error:make(unrecognized_client),
                            {_, NextReq} = rinamo_response:send(ErrorMsg#error.http_code, rinamo_error:format(ErrorMsg), Req),
                            {halt, NextReq};
                        _ ->
                            assign_data_owner(OwnerKey, Req, Env)
                    end;
                _ ->
                    % observed that sometimes AWS java client sends <<>> or <<"T">>
                    % may be related to:  https://github.com/extend/cowboy/issues/448
                    lager:warning("Invalid Method: ~p~n", [Method]),
                    ErrorMsg = rinamo_error:make(incomplete_signature),
                    {_, NextReq} = rinamo_response:send(ErrorMsg#error.http_code, rinamo_error:format(ErrorMsg), Req),
                    {halt, NextReq}
            end
    end.

%% Internal
execute_auth_handler(AuthToken, Req) ->
    {AccessKey, Signature} = tokenize_auth_header(binary_to_list(AuthToken)),
    lager:debug("AccessKey: ~p~n", [AccessKey]),
    lager:debug("Signature: ~p~n", [Signature]),

    % ------- begin auth concern

    M = rinamo_config:get_auth_strategy(),
    F = authorize,
    A = [AccessKey, Signature, Req],
    lager:debug("Auth Strat: ~p~n", [M]),
    erlang:apply(M, F, A).

    % ------- end auth concern

assign_data_owner(OwnerKey, Req, Env) ->
    {_, {_, HandlerOpts}, PartialEnv} = lists:keytake(handler_opts, 1, Env),
    State = case HandlerOpts of
        [] -> #state{user_key = OwnerKey};
        _ -> HandlerOpts#state{user_key = OwnerKey}
    end,

    NewEnv = [{handler_opts, State} | PartialEnv],

    {ok, Req, NewEnv}.


tokenize_auth_header(HeaderValue) ->
    case HeaderValue of
      undefined -> undefined;
      _ ->
        [_, Credentials, _, SignatureVal] = string:tokens(HeaderValue, " "),
        AK_StartCharPos = string:str(Credentials, "="),
        EndCharPos = string:str(Credentials, "/"),
        AccessKey = string:substr(Credentials, AK_StartCharPos + 1, EndCharPos - (AK_StartCharPos + 1)),
        Sig_StartCharPos = string:str(SignatureVal, "="),
        Signature = re:replace(string:substr(SignatureVal, Sig_StartCharPos + 1), "\\s+$", "", [global,{return,list}]),
        {list_to_binary(AccessKey), list_to_binary(Signature)}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

auth_fixture() ->
    {_, Fixture} = file:read_file("../tests/fixtures/access_key.txt"),
    binary_to_list(Fixture).

tokenize_auth_header_test() ->
    Input = auth_fixture(),
    Actual = tokenize_auth_header(Input),
    Expected = {
        <<"RANDY_ACCESS_KEY">>,
        <<"81f71d83f35b3b2be9589f9ec0f5edca95b14d602f639b183e729d1fd1e3308c">>
    },
    ?assertEqual(Expected, Actual),
    ?assertEqual(undefined, tokenize_auth_header(undefined)).

-endif.
