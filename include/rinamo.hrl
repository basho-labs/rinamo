-record(state, {
	user_key :: binary()
}).

-record(error, {
    http_code :: integer(),
    code :: binary(),
    message :: binary()
}).

-define(RINAMO_SEPARATOR, 16#1f).

-define(AMZ_AUTH_HEADER,<<"authorization">>).
-define(AMZ_OP_HEADER,<<"x-amz-target">>).