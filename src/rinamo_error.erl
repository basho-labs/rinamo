-module(rinamo_error).

-include("rinamo.hrl").

-export([make/1, format/1]).

build_error(HttpCode, Code, Message) ->
    #error{http_code = HttpCode, code = Code, message = Message}.

make(access_denied) ->
    build_error(400, 
        <<"AccessDeniedException">>, 
        <<"Access denied.">>);

make(conditional_check_failed) ->
    build_error(400,
        <<"ConditionalCheckFailedException">>, 
        <<"The conditional request failed.">>);

make(incomplete_signature) ->
    build_error(400,
        <<"IncompleteSignatureException">>, 
        <<"The request signature does not conform to AWS standards.">>);

make(item_collection_size_limit_exceeded) ->
    build_error(400, 
        <<"ItemCollectionSizeLimitExceededException">>, 
        <<"Collection size exceeded.">>);

make(limit_exceeded) ->
    build_error(400, 
        <<"LimitExceededException">>, 
        <<"Too many operations for a given subscriber.">>);

make(missing_authentication_token) ->
    build_error(400, 
        <<"MissingAuthenticationTokenException">>, 
        <<"Request must contain a valid (registered) AWS Access Key ID.">>);

make(provisioned_throughput_exceeded) ->
    build_error(400, 
        <<"ProvisionedThroughputExceededException">>, 
        <<"You exceeded your maximum allowed provisioned throughput for a table or for one or more global secondary indexes. To view performance metrics for provisioned throughput vs. consumed throughput, go to the Amazon CloudWatch console.">>);

make(resource_in_use) ->
    build_error(400, 
        <<"ResourceInUseException">>, 
        <<"The resource which you are attempting to change is in use.">>);

make(resource_not_found) ->
    build_error(400, 
        <<"ResourceNotFoundException">>, 
        <<"The resource which is being requested does not exist.">>);

make(throttling) ->
    build_error(400, 
        <<"ThrottlingException">>, 
        <<"Rate of requests exceeds the allowed throughput.">>);

make(unrecognized_client) ->
    build_error(400, 
        <<"UnrecognizedClientException">>, 
        <<"The Access Key ID or security token is invalid.">>);

make(validation) ->
    build_error(400, 
        <<"ValidationException">>, 
        <<"One or more required parameter values were missing.">>);

make(request_too_large) ->
    build_error(413, 
        <<"">>, 
        <<"Request Entity Too Large.">>);

make(internal_failure) ->
    build_error(500, 
        <<"InternalFailure">>, 
        <<"The server encountered an internal error trying to fulfill the request.">>);

make(internal_server_error) ->
    build_error(500, 
        <<"InternalServerError">>, 
        <<"The server encountered an internal error trying to fulfill the request.">>);

make(service_unavailable) ->
    build_error(500, 
        <<"ServiceUnavailableException">>, 
        <<"The service is currently unavailable or busy.">>).

format(Error) ->
    "{\"__type\":\"com.amazonaws.dynamodb.v20120810#" ++ Error#error.code ++ "\",\"Message\":\"" ++ Error#error.message ++ "\"}".

