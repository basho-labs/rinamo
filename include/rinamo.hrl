-record(state, {
	user_key :: binary()
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
