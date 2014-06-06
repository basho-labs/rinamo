-module(rinamo_auth_keystone_v2).

-export([authorize/3]).

% Utilize Keystone to verify access using the
% AWS V4 Request Signing HMac Process
%
% If authorization passes, this function will return the project id
% as the primary data owner.
authorize(AccessKey, Signature, Req) ->
    KeyStoneBaseURL = rinamo_config:get_keystone_baseurl(),
    KeyStoneCType = "application/json",
    KeyStoneAuthPath = "/v2.0/ec2tokens",

    {Host, _} = cowboy_req:header(<<"host">>, Req),
    {Path, _} = cowboy_req:path(Req),
    {Method, _} = cowboy_req:method(Req),
    {Headers, _} = cowboy_req:headers(Req),
    {_, Body, _} = cowboy_req:body(Req),
    Hash = crypto:hash(sha256, Body),
    Digest = lists:flatten([integer_to_list(X, 16) || <<X:4>> <= Hash]),
    ContentSHA256 = erlang:list_to_binary(string:to_lower(Digest)),

    % Capitalize Headers b/c Keystone
    % "Field names are case-insensitive." - RFC 2616
    AuthJson = rinamo_codec:encode_awsv4_hmac(
        AccessKey, Signature, Host, Method, Path,
        uppercase(Headers), ContentSHA256),

    lager:debug("Keystone Request: ~p~n", [AuthJson]),
    URI = binary:bin_to_list(KeyStoneBaseURL) ++ KeyStoneAuthPath,
    Response = ibrowse:send_req(URI, [{"Content-Type", KeyStoneCType}], post, AuthJson),
    lager:debug("Keystone Response: ~p~n", [Response]),
    case Response of
        {ok, "200", _, ResponseBody} ->
            KeystoneJsonV2 = jsx:decode(list_to_binary(ResponseBody)),
            % return the project / tenant id as the data owner.
            kvc:path("access.token.tenant.id", KeystoneJsonV2);
        _ -> none
    end.

% Internal
uppercase(Headers) ->
    Fun = fun(LowerCasedKey) ->
        HeaderKey = binary:bin_to_list(LowerCasedKey),
        erlang:list_to_binary(capitalize(HeaderKey))
    end,
    lists:keymap(Fun, 1, Headers).

capitalize(S) ->
    F = fun([H|T]) -> [string:to_upper(H) | string:to_lower(T)] end,
    string:join(lists:map(F, string:tokens(S, "-")), "-").
