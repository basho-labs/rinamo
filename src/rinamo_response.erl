%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------

-module(rinamo_response).

-export([make/1, send/3]).

-include("rinamo.hrl").

make(Data) ->
    Json = jsx:encode(Data),
    Crc32 = erlang:crc32(Json),
    {ok, Json, Crc32}.

send(Status, ResponseBody, Req) ->
    {ok, Json, Crc32} = make(ResponseBody),
    ReqWcrc32 = cowboy_req:set_resp_header(?AMZ_CRC32_HEADER, integer_to_binary(Crc32), Req),
    cowboy_req:reply(Status, [{<<"content-type">>, <<"application/json">>}], Json, ReqWcrc32).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_test() ->
    Input = [{<<"Some_Json_Attribute">>, <<"Some Json Value">>}],
    {ok, Json, Crc32} = make(Input),
    ?assertEqual(560185682, Crc32),
    ?assertEqual(<<"{\"Some_Json_Attribute\":\"Some Json Value\"}">>, Json).

-endif.
