-record(state, {
	user_key :: binary(),
	operation :: binary(),
	aws_api_version :: binary()
}).

-record(error, {
    http_code :: integer(),
    code :: binary(),
    message :: binary()
}).

-define(RINAMO_SEPARATOR, 16#1f).
-define(AMZ_CRC32_HEADER, <<"x-amz-crc32">>).
