-module(rinamo_codec).

-export([
    decode_create_table/1,
    decode_table_name/1,
    decode_list_tables/1,
    decode_put_item/1,
    decode_item_request/1,
    decode_query/1,
    decode_rinamo_keypath/2]).

-include("rinamo.hrl").

decode_rinamo_keypath([], Acc) ->
    Acc;
decode_rinamo_keypath(Binary, Acc) when is_binary(Binary) ->
    List = binary:bin_to_list(Binary),
    decode_rinamo_keypath(List, Acc);
decode_rinamo_keypath(List, Acc) when is_list(List) ->
    case lists:splitwith(fun(X) -> X /= ?RINAMO_SEPARATOR end, List) of
        {First, [_ | Last]} ->
            decode_rinamo_keypath(Last, lists:append(Acc, [First]));
        {Last, []} ->
            decode_rinamo_keypath([], lists:append(Acc, [Last]))
    end.

decode_batch_get_item(Json) ->
    ok.

decode_batch_write_item(Json) ->
    ok.

decode_list_tables(Json) ->
    Limit = kvc:path("Limit", Json),
    ExclusiveStart = kvc:path("ExclusiveStartTableName", Json),
    [{exclusive_start, ExclusiveStart},
     {limit, Limit}].

decode_item_request(Json) ->
    Attributes = kvc:path("AttributesToGet", Json),
    ConsistentRead = kvc:path("ConsistentRead", Json),
    KeyValues = kvc:path("Key", Json),
    Keys = decode_keys(KeyValues, []),
    ReturnConsumedCapacity = kvc:path("ReturnConsumedCapacity", Json),
    TableName = kvc:path("TableName", Json),
    [{attributes_to_get, Attributes},
     {consistent_read, ConsistentRead},
     {keys, Keys},
     {return_consumed_capacity, ReturnConsumedCapacity},
     {tablename, TableName}].

decode_put_item(Json) ->
    TableName = kvc:path("TableName", Json),
    Expected = decode_put_expected(kvc:path("Expected", Json), []),
    Item = kvc:path("Item", Json),
    ReturnConsumedCapacity = kvc:path("ReturnConsumedCapacity", Json),
    ReturnItemCollectionMetrics = kvc:path("ReturnItemCollectionMetrics", Json),
    ReturnValues = kvc:path("ReturnValues", Json),

    [{expected, Expected},
     {item, Item},
     {return_consumed_capacity, ReturnConsumedCapacity},
     {return_item_collection_metrics, ReturnItemCollectionMetrics},
     {return_values, ReturnValues},
     {tablename, TableName}].

decode_update_item(Json) ->
    ok.

decode_delete_item(Json) ->
    ok.

decode_table_name(Json) ->
    TableName = kvc:path("TableName", Json),
    [{tablename, TableName}].

decode_create_table(Json) ->
    TableName = kvc:path("TableName", Json),
    AttributeDefinitions = decode_table_attributes(kvc:path("AttributeDefinitions", Json), []),
    KeySchema = decode_2i_key_schema(kvc:path("KeySchema", Json), []),
    SecondaryIndexes = decode_2i(kvc:path("LocalSecondaryIndexes", Json), []),
    ProvisionedThroughput = [{<<"ReadCapacityUnits">>, kvc:path("ProvisionedThroughput.ReadCapacityUnits", Json)},
                             {<<"WriteCapacityUnits">>, kvc:path("ProvisionedThroughput.WriteCapacityUnits", Json)}],

    [{tablename, TableName},
     {fields, AttributeDefinitions},
     {key_schema, KeySchema},
     {lsi, SecondaryIndexes},
     {provisioned_throughput, ProvisionedThroughput},
     {raw_schema, Json}].

decode_update_table(Json) ->
    ok.

decode_query(Json) ->
    AttributesToGet = kvc:path("AttributesToGet", Json),
    ConsistentRead = kvc:path("ConsistentRead", Json),
    ExclusiveStartKey = kvc:path("ExclusiveStartKey", Json),
    IndexName = kvc:path("IndexName", Json),
    KeyConditionsArr = kvc:path("KeyConditions", Json),
    KeyConditions = decode_key_conditions(KeyConditionsArr, []),
    Limit = kvc:path("Limit", Json),
    ReturnConsumedCapacity = kvc:path("ReturnConsumedCapacity", Json),
    ScanIndexForward = kvc:path("ScanIndexForward", Json),
    Select = kvc:path("Select", Json),
    TableName = kvc:path("TableName", Json),

    [{attributes_to_get, AttributesToGet},
     {consistent_read, ConsistentRead},
     {exclusive_start, ExclusiveStartKey},
     {index_name, IndexName},
     {key_conditions, KeyConditions},
     {limit, Limit},
     {return_consumed_capacity, ReturnConsumedCapacity},
     {scan_index_forward, ScanIndexForward},
     {select, Select},
     {tablename, TableName}].

decode_scan(Json) ->
    ok.

%% Internal

decode_keys([], Acc) ->
    lists:reverse(Acc);
decode_keys([{KeyName, KeyValue}|Rest], Acc) ->
    KeyValues = decode_key_value(KeyValue, []),
    KeyTypeValue = case length(KeyValues) of
        1 -> {binary:bin_to_list(KeyName), lists:nth(1, KeyValues)};
        _ -> {error, too_many_key_values}
    end,
    decode_keys(Rest, [KeyTypeValue|Acc]).


decode_key_value([], Acc) ->
    lists:reverse(Acc);
decode_key_value([{FieldType, FieldValue}|Rest], Acc) ->
    decode_keys(Rest, [{FieldType, FieldValue}|Acc]).


decode_put_expected([{}], _) ->
    [{}];
decode_put_expected([], Acc) ->
    lists:reverse(Acc);
decode_put_expected([Field|Rest], Acc) ->
    % TODO:  Fix order match that depends on array order
    {FieldName, Expectation} = Field,
    {ExpectedData, FieldType, FieldValue} = case Expectation of
        [{<<"Exists">>, E}, {<<"Value">>, [{FT, FV}]}] ->
            {E, FT, FV};
        [{<<"Value">>, [{FT, FV}]}, {<<"Exists">>, E}] ->
            {E, FT, FV};
        _ -> {undefined, undefined, undefined}
    end,
    ExpectedAtom = erlang:list_to_atom(string:to_lower(erlang:binary_to_list(ExpectedData))),
    decode_put_expected(Rest, [{FieldName, [{<<"Exists">>, ExpectedAtom}, {FieldType, FieldValue}]} | Acc]).


decode_table_attributes([], Acc) ->
    lists:reverse(Acc);
decode_table_attributes([Attribute|Rest], Acc) ->
    decode_table_attributes(Rest, [[{<<"AttributeName">>, kvc:path("AttributeName", Attribute)},
                                    {<<"AttributeType">>, kvc:path("AttributeType", Attribute)}] | Acc]).

decode_2i([], Acc) ->
    lists:reverse(Acc);
decode_2i([Index|Rest], Acc) ->
    IndexName = kvc:path("IndexName", Index),
    KeySchema = decode_2i_key_schema(kvc:path("KeySchema", Index), []),
    Projection = [{non_key_attributes, kvc:path("Projection.NonKeyAttributes", Index)},
                  {projection_type, kvc:path("Projection.ProjectionType", Index)}],

    IndexResult = [{IndexName,
             [{key_schema, KeySchema},
              {projection, Projection}]}],
    decode_2i(Rest, [IndexResult|Acc]).


decode_2i_key_schema([], Acc) ->
    lists:reverse(Acc);
decode_2i_key_schema([Attribute|Rest], Acc) ->
    decode_2i_key_schema(Rest, [[{<<"AttributeName">>, kvc:path("AttributeName", Attribute)},
                                 {<<"KeyType">>, kvc:path("KeyType", Attribute)}] | Acc]).

decode_key_conditions([], Acc) ->
    lists:reverse(Acc);
decode_key_conditions([Condition|Rest], Acc) ->
    {Key, Args} = Condition,
    AttributeValues = decode_attribute_values(kvc:path("AttributeValueList", Args), []),
    ComparisonOperator = kvc:path("ComparisonOperator", Args),
    decode_key_conditions(Rest, [{Key, AttributeValues, ComparisonOperator}|Acc]).


decode_attribute_values([], Acc) ->
    lists:reverse(Acc);
decode_attribute_values([Attribute|Rest], Acc) ->
    [{Type, Value}] = Attribute,
    decode_attribute_values(Rest, [{Type, Value}|Acc]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decode_batch_get_item_test() ->
    Actual = decode_batch_get_item([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).

decode_batch_write_item_test() ->
    Actual = decode_batch_write_item([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).

decode_item_request_test() ->
    Json_Bin = <<"{
    \"AttributesToGet\": [
        \"string\"
    ],
    \"ConsistentRead\": false,
    \"Key\":
        {
            \"key1\" : {\"B\": \"blob\"},
            \"key2\" : {\"BS\": [\"blob\"]},
            \"key3\" : {\"N\": \"string\"},
            \"key4\" : {\"NS\": [\"string\"]},
            \"key5\" : {\"S\": \"string\"},
            \"key6\" : {\"SS\": [\"string\"]}
        },
    \"ReturnConsumedCapacity\": \"string\",
    \"TableName\": \"string\"
    }">>,
    Actual = decode_item_request(jsx:decode(Json_Bin)),
    io:format("~p", [Actual]),

    Expected = [{attributes_to_get,[<<"string">>]},
                {consistent_read, false},
                {keys, [{"key1",{<<"B">>,<<"blob">>}},
                        {"key2",{<<"BS">>,[<<"blob">>]}},
                        {"key3",{<<"N">>,<<"string">>}},
                        {"key4",{<<"NS">>,[<<"string">>]}},
                        {"key5",{<<"S">>,<<"string">>}},
                        {"key6",{<<"SS">>,[<<"string">>]}}]},
                {return_consumed_capacity,<<"string">>},
                {tablename,<<"string">>}],

    ?assertEqual(Expected, Actual).

decode_put_item_test() ->
    Json_Bin = <<"{
    \"Expected\":
        {
            \"field1\": {\"Exists\": \"true\", \"Value\": {\"B\": \"blob\"}},
            \"field2\": {\"Exists\": \"false\", \"Value\": {\"BS\": [ \"blob\" ]}},
            \"field3\": {\"Exists\": \"true\", \"Value\": {\"N\": \"string\"}},
            \"field4\": {\"Exists\": \"false\", \"Value\": {\"NS\": [ \"string\" ]}},
            \"field5\": {\"Exists\": \"true\", \"Value\": {\"S\": \"string\"}},
            \"field6\": {\"Exists\": \"false\", \"Value\": {\"SS\": [ \"string\" ]}}
        },
    \"Item\":
        {
            \"field1\": {\"B\": \"blob\"},
            \"field2\": {\"BS\": [\"blob\"]},
            \"field3\": {\"N\": \"string\"},
            \"field4\": {\"NS\": [\"string\"]},
            \"field5\": {\"S\": \"string\"},
            \"field6\": {\"SS\": [\"string\"]}
        },
    \"ReturnConsumedCapacity\": \"string\",
    \"ReturnItemCollectionMetrics\": \"string\",
    \"ReturnValues\": \"string\",
    \"TableName\": \"table_name\"
    }">>,
    Actual = decode_put_item(jsx:decode(Json_Bin)),
    Expected = [
        {expected, [
            {<<"field1">>, [{<<"Exists">>, true}, {<<"B">>, <<"blob">>}]},
            {<<"field2">>, [{<<"Exists">>, false}, {<<"BS">>, [<<"blob">>]}]},
            {<<"field3">>, [{<<"Exists">>, true}, {<<"N">>, <<"string">>}]},
            {<<"field4">>, [{<<"Exists">>, false}, {<<"NS">>, [<<"string">>]}]},
            {<<"field5">>, [{<<"Exists">>, true}, {<<"S">>, <<"string">>}]},
            {<<"field6">>, [{<<"Exists">>, false}, {<<"SS">>, [<<"string">>]}]}
        ]},
        {item,[{<<"field1">>,[{<<"B">>,<<"blob">>}]},
            {<<"field2">>,[{<<"BS">>,[<<"blob">>]}]},
            {<<"field3">>,[{<<"N">>,<<"string">>}]},
            {<<"field4">>,[{<<"NS">>,[<<"string">>]}]},
            {<<"field5">>,[{<<"S">>,<<"string">>}]},
            {<<"field6">>,[{<<"SS">>,[<<"string">>]}]}]},
        {return_consumed_capacity, <<"string">>},
        {return_item_collection_metrics, <<"string">>},
        {return_values, <<"string">>},
        {tablename, <<"table_name">>}],
    io:format("Actual: ~p~n", [Actual]),
    ?assertEqual(Expected, Actual).

decode_update_item_test() ->
    Actual = decode_update_item([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).

decode_delete_item_test() ->
    Actual = decode_delete_item([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).

decode_table_name_test() ->
    Json_Bin = <<"{\"TableName\": \"table_name\"}">>,
    Actual = decode_table_name(jsx:decode(Json_Bin)),
    io:format("Actual: ~p~n", [Actual]),
    Expected = [{tablename, <<"table_name">>}],
    ?assertEqual(Expected, Actual).

decode_create_table_test() ->
    Json_Bin = <<"{
    \"AttributeDefinitions\": [
        {
            \"AttributeName\": \"Id\",
            \"AttributeType\": \"N\"
        },
        {
            \"AttributeName\": \"Date\",
            \"AttributeType\": \"S\"
        }
    ],
    \"KeySchema\": [
        {
            \"AttributeName\": \"Id\",
            \"KeyType\": \"HASH\"
        },
        {
            \"AttributeName\": \"Date\",
            \"KeyType\": \"RANGE\"
        }
    ],
    \"LocalSecondaryIndexes\": [
        {
            \"IndexName\": \"2i_name\",
            \"KeySchema\": [
                {
                    \"AttributeName\": \"attr_name\",
                    \"KeyType\": \"key_type\"
                }
            ],
            \"Projection\": {
                \"NonKeyAttributes\": [
                    \"attr_name\"
                ],
                \"ProjectionType\": \"projection_type\"
            }
        }
    ],
    \"ProvisionedThroughput\": {
        \"ReadCapacityUnits\": 20,
        \"WriteCapacityUnits\": 5
    },
    \"TableName\": \"table_name\"
    }">>,
    Actual = decode_create_table(jsx:decode(Json_Bin)),
    io:format("Actual: ~p~n", [Actual]),
    Expected = [{tablename, <<"table_name">>},
                {fields,[
                    [{<<"AttributeName">>, <<"Id">>}, {<<"AttributeType">>, <<"N">>}],
                    [{<<"AttributeName">>, <<"Date">>}, {<<"AttributeType">>, <<"S">>}]
                ]},
                {key_schema, [
                    [{<<"AttributeName">>, <<"Id">>}, {<<"KeyType">>, <<"HASH">>}],
                    [{<<"AttributeName">>, <<"Date">>}, {<<"KeyType">>, <<"RANGE">>}]
                ]},
                {lsi, [[{<<"2i_name">>,
                        [{key_schema, [
                            [{<<"AttributeName">>, <<"attr_name">>}, {<<"KeyType">>, <<"key_type">>}]
                        ]},
                        {projection, [{non_key_attributes, [<<"attr_name">>]},
                                      {projection_type, <<"projection_type">>}]}]
                }]]},
                {provisioned_throughput, [{<<"ReadCapacityUnits">>, 20},
                                          {<<"WriteCapacityUnits">>, 5}]},
                {raw_schema, jsx:decode(Json_Bin)}
                ],
    ?assertEqual(Expected, Actual).

decode_update_table_test() ->
    Actual = decode_update_table([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).


decode_query_test() ->
    Json_Bin = <<"{
    \"AttributesToGet\": [
        \"string\"
    ],
    \"ConsistentRead\": false,
    \"ExclusiveStartKey\":
        {
            \"Key1\": {\"B\": \"blob\"},
            \"Key2\": {\"BS\": [\"blob\"]},
            \"Key3\": {\"N\": \"string\"},
            \"Key4\": {\"NS\": [\"string\"]},
            \"Key5\": {\"S\": \"string\"},
            \"Key6\": {\"SS\": [\"string\"]}
        },
    \"IndexName\": \"string\",
    \"KeyConditions\":
        {
        \"Key1\": {
            \"AttributeValueList\": [{\"B\": \"blob\"}],
            \"ComparisonOperator\": \"string\"},
        \"Key2\": {
            \"AttributeValueList\": [{\"BS\": [\"blob\"]}],
            \"ComparisonOperator\": \"string\"},
        \"Key3\": {
            \"AttributeValueList\": [{\"N\": \"string\"}],
            \"ComparisonOperator\": \"string\"},
        \"Key4\": {
            \"AttributeValueList\": [{\"NS\": [\"string\"]}],
            \"ComparisonOperator\": \"string\"},
        \"Key5\": {
            \"AttributeValueList\": [{\"S\": \"string\"}],
            \"ComparisonOperator\": \"string\"},
        \"Key6\": {
            \"AttributeValueList\": [{\"SS\": [\"string\"]}],
            \"ComparisonOperator\": \"string\"}
        },
    \"Limit\": \"number\",
    \"ReturnConsumedCapacity\": \"string\",
    \"ScanIndexForward\": false,
    \"Select\": \"string\",
    \"TableName\": \"string\"
    }">>,
    Actual = decode_query(jsx:decode(Json_Bin)),
    Expected = [{attributes_to_get, [<<"string">>]},
                {consistent_read, false},
                {exclusive_start, [
                    {<<"Key1">>, [{<<"B">>, <<"blob">>}]},
                    {<<"Key2">>, [{<<"BS">>, [<<"blob">>]}]},
                    {<<"Key3">>, [{<<"N">>, <<"string">>}]},
                    {<<"Key4">>, [{<<"NS">>, [<<"string">>]}]},
                    {<<"Key5">>, [{<<"S">>, <<"string">>}]},
                    {<<"Key6">>, [{<<"SS">>, [<<"string">>]}]}
                ]},
                {index_name, <<"string">>},
                {key_conditions, [
                    {<<"Key1">>, [{<<"B">>, <<"blob">>}], <<"string">>},
                    {<<"Key2">>, [{<<"BS">>, [<<"blob">>]}], <<"string">>},
                    {<<"Key3">>, [{<<"N">>, <<"string">>}], <<"string">>},
                    {<<"Key4">>, [{<<"NS">>, [<<"string">>]}], <<"string">>},
                    {<<"Key5">>, [{<<"S">>, <<"string">>}], <<"string">>},
                    {<<"Key6">>, [{<<"SS">>, [<<"string">>]}], <<"string">>}
                ]},
                {limit, <<"number">>},
                {return_consumed_capacity, <<"string">>},
                {scan_index_forward, false},
                {select, <<"string">>},
                {tablename, <<"string">>}],
    io:format("Actual: ~p", [Actual]),
    ?assertEqual(Expected, Actual).

decode_scan_test() ->
    Expected = ok,
    Actual = decode_scan(undefined),
    ?assertEqual(Expected, Actual).

-endif.
