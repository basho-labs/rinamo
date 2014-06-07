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

-record(state, {
	owner_key :: binary(),
	body :: binary()
}).

-record(error, {
    http_code :: integer(),
    code :: binary(),
    message :: binary()
}).

-define(AMZ_CRC32_HEADER, <<"x-amz-crc32">>).
-define(AMZ_OP_HEADER,<<"x-amz-target">>).

-define(RINAMO_SEPARATOR, 16#1f).
-define(RINAMO_COUNTER_TYPE, <<"rinamo_counter_bucket_type">>).
-define(RINAMO_SET_TYPE, <<"rinamo_set_bucket_type">>).
-define(RINAMO_CRDT_MAP, [{riak_dt_orswot, ?RINAMO_SET_TYPE},
                          {riak_dt_pncounter, ?RINAMO_COUNTER_TYPE}]).
