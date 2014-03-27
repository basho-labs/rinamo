-record(state, {
	user_key :: binary()
}).

-record(error, {
    http_code :: integer(),
    code :: binary(),
    message :: binary()
}).

-define(RINAMO_SEPARATOR, 16#1f).
-define(AMZ_CRC32_HEADER, <<"x-amz-crc32">>).
-define(AMZ_OP_HEADER,<<"x-amz-target">>).
