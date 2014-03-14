-module(rinamo_kv).

-export([client/0]).
-export([get/3, put/5, delete/3]).

-spec client() -> any().
client() ->
  {ok,C} = riak:local_client(),
  C.

-spec get(any(), binary(), binary()) -> any().
get(Client, Bucket, Key) ->
  case Client:get(Bucket, Key) of
    {ok, O} ->
      {value, riak_object:get_value(O)};
    Other ->
      Other
  end.

-spec put(any(), binary(), binary(), binary(), string()) -> ok.
put(Client, Bucket, Key, Value, ContentType) ->
  O = riak_object:new(Bucket, Key, Value, ContentType),
  Client:put(O).

-spec delete(any(), binary(), binary()) -> ok.
delete(Client, Bucket, Key) ->
  Client:delete(Bucket, Key).
