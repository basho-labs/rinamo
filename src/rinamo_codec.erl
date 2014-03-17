-module(rinamo_codec).

-export([decode_create_table/1, decode_describe_table/1,
         decode_list_tables/1, decode_put_item/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

decode_batch_get_item(Request) ->
    ok.

decode_batch_write_item(Request) ->
    ok.

decode_list_tables(Json) ->
  Limit = kvc:path("Limit", Json),
  ExclusiveStart = kvc:path("ExclusiveStartTableName", Json),
  [{exclusive_start, ExclusiveStart},
   {limit, Limit}].

decode_get_item(Request) ->
    Attributes = lists:map(fun(X) -> binary:bin_to_list(X) end, kvc:path("AttributesToGet", Request)),
    ConsistentRead = binary:bin_to_list(kvc:path("ConsistentRead", Request)),
    {struct, KeyValues} = kvc:path("Key", Request),
    Keys = decode_keys(KeyValues, []),
    ReturnConsumedCapacity = binary:bin_to_list(kvc:path("ReturnConsumedCapacity", Request)),
    TableName = binary:bin_to_list(kvc:path("TableName", Request)),
    [{"AttributesToGet", Attributes},
     {"ConsistentRead", ConsistentRead},
     {"Keys", Keys},
     {"ReturnConsumedCapacity", ReturnConsumedCapacity},
     {"TableName", TableName}].

encode_get_item_response(Response) ->
    Data = {struct,[{<<"ConsumedCapacity">>,
                    {struct,[{<<"CapacityUnits">>,<<"number">>},
                             {<<"GlobalSecondaryIndexes">>,
                              {struct,[{<<"string">>,
                                        {struct,[{<<"CapacityUnits">>,
                                                  <<"number">>}]}}]}},
                             {<<"LocalSecondaryIndexes">>,
                              {struct,[{<<"string">>,
                                        {struct,[{<<"CapacityUnits">>,
                                                  <<"number">>}]}}]}},
                             {<<"Table">>,{struct,[{<<"CapacityUnits">>,<<"number">>}]}},
                             {<<"TableName">>,<<"string">>}]}},
                    {<<"Item">>,
                     {struct,[{<<"string">>,
                               {struct,[{<<"B">>,<<"blob">>},
                                        {<<"BS">>,[<<"blob">>]},
                                        {<<"N">>,<<"string">>},
                                        {<<"NS">>,[<<"string">>]},
                                        {<<"S">>,<<"string">>},
                                        {<<"SS">>,[<<"string">>]}]}}]}}]},

    mochijson2:encode(Data).

decode_put_item(Json) ->
    TableName = kvc:path("TableName", Json),
    Expected = decode_put_expected(kvc:path("Expected", Json), []),
    Item = decode_put_item(kvc:path("Item", Json), []),
    ReturnConsumedCapacity = kvc:path("ReturnConsumedCapacity", Json),
    ReturnItemCollectionMetrics = kvc:path("ReturnItemCollectionMetrics", Json),
    ReturnValues = kvc:path("ReturnValues", Json),

    [{expected, Expected},
     {item, Item},
     {return_consumed_capacity, ReturnConsumedCapacity},
     {return_item_collection_metrics, ReturnItemCollectionMetrics},
     {return_values, ReturnValues},
     {tablename, TableName}].

encode_put_item_response(Response) ->
    Data = {struct,[{<<"Attributes">>,
          {struct,[{<<"string">>,
                    {struct,[{<<"B">>,<<"blob">>},
                             {<<"BS">>,[<<"blob">>]},
                             {<<"N">>,<<"string">>},
                             {<<"NS">>,[<<"string">>]},
                             {<<"S">>,<<"string">>},
                             {<<"SS">>,[<<"string">>]}]}}]}},
         {<<"ConsumedCapacity">>,
          {struct,[{<<"CapacityUnits">>,<<"number">>},
                   {<<"GlobalSecondaryIndexes">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"CapacityUnits">>,
                                        <<"number">>}]}}]}},
                   {<<"LocalSecondaryIndexes">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"CapacityUnits">>,
                                        <<"number">>}]}}]}},
                   {<<"Table">>,{struct,[{<<"CapacityUnits">>,<<"number">>}]}},
                   {<<"TableName">>,<<"string">>}]}},
         {<<"ItemCollectionMetrics">>,
          {struct,[{<<"ItemCollectionKey">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"B">>,<<"blob">>},
                                       {<<"BS">>,[<<"blob">>]},
                                       {<<"N">>,<<"string">>},
                                       {<<"NS">>,[<<"string">>]},
                                       {<<"S">>,<<"string">>},
                                       {<<"SS">>,[<<"string">>]}]}}]}},
                   {<<"SizeEstimateRangeGB">>,[<<"number">>]}]}}]},

    mochijson2:encode(Data).


decode_update_item(Request) ->
    ok.

decode_delete_item(Request) ->
    ok.

decode_describe_table(Json) ->
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

decode_update_table(Request) ->
    ok.

decode_delete_table(Request) ->
    ok.

decode_query(Request) ->
    AttributesToGet = lists:map(fun(X) -> binary:bin_to_list(X) end, kvc:path("AttributesToGet", Request)),
    ConsistentRead = binary:bin_to_list(kvc:path("ConsistentRead", Request)),
    {struct, ExStartKeys} = kvc:path("ExclusiveStartKey", Request),
    ExclusiveStartKey = decode_put_item(ExStartKeys, []),
    IndexName = binary:bin_to_list(kvc:path("IndexName", Request)),
    {struct, KeyConditionsArr} = kvc:path("KeyConditions", Request),
    KeyConditions = decode_key_conditions(KeyConditionsArr, []),
    Limit = binary:bin_to_list(kvc:path("Limit", Request)),
    ReturnConsumedCapacity = binary:bin_to_list(kvc:path("ReturnConsumedCapacity", Request)),
    ScanIndexForward = binary:bin_to_list(kvc:path("ScanIndexForward", Request)),
    Select = binary:bin_to_list(kvc:path("Select", Request)),
    TableName = binary:bin_to_list(kvc:path("TableName", Request)),

    [{"AttributesToGet", AttributesToGet},
     {"ConsistentRead", ConsistentRead},
     {"ExclusiveStartKey", ExclusiveStartKey},
     {"IndexName", IndexName},
     {"KeyConditions", KeyConditions},
     {"Limit", Limit},
     {"ReturnConsumedCapacity", ReturnConsumedCapacity},
     {"ScanIndexForward", ScanIndexForward},
     {"Select", Select},
     {"TableName", TableName}].

encode_query_response(Response) ->
    Data = {struct,[{<<"ConsumedCapacity">>,
          {struct,[{<<"CapacityUnits">>,<<"number">>},
                   {<<"GlobalSecondaryIndexes">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"CapacityUnits">>,
                                        <<"number">>}]}}]}},
                   {<<"LocalSecondaryIndexes">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"CapacityUnits">>,
                                        <<"number">>}]}}]}},
                   {<<"Table">>,{struct,[{<<"CapacityUnits">>,<<"number">>}]}},
                   {<<"TableName">>,<<"string">>}]}},
         {<<"Count">>,<<"number">>},
         {<<"Items">>,
          [{struct,[{<<"string">>,
                     {struct,[{<<"B">>,<<"blob">>},
                              {<<"BS">>,[<<"blob">>]},
                              {<<"N">>,<<"string">>},
                              {<<"NS">>,[<<"string">>]},
                              {<<"S">>,<<"string">>},
                              {<<"SS">>,[<<"string">>]}]}}]}]},
         {<<"LastEvaluatedKey">>,
          {struct,[{<<"string">>,
                    {struct,[{<<"B">>,<<"blob">>},
                             {<<"BS">>,[<<"blob">>]},
                             {<<"N">>,<<"string">>},
                             {<<"NS">>,[<<"string">>]},
                             {<<"S">>,<<"string">>},
                             {<<"SS">>,[<<"string">>]}]}}]}}]},

     mochijson2:encode(Data).


decode_scan(Request) ->
    ok.


%% Internal

decode_keys([], Acc) ->
    lists:reverse(Acc);
decode_keys([{KeyName, {struct, KeyValue}}|Rest], Acc) ->
    KeyValues = decode_key_value(KeyValue, []),
    KeyTypeValue = case length(KeyValues) of
        1 -> {binary:bin_to_list(KeyName), lists:nth(1, KeyValues)};
        _ -> {error, too_many_key_values}
    end,
    decode_keys(Rest, [KeyTypeValue|Acc]).


decode_key_value([], Acc) ->
    lists:reverse(Acc);
decode_key_value([{FieldType, FieldValue}|Rest], Acc) ->
    ValueList = case is_list(FieldValue) of
        true -> lists:map(fun(X) -> binary:bin_to_list(X) end, FieldValue);
        _ -> binary:bin_to_list(FieldValue)
    end,
    decode_keys(Rest, [{binary:bin_to_list(FieldType), ValueList}|Acc]).


decode_put_expected([{}], Acc) ->
    [{}];
decode_put_expected([], Acc) ->
    lists:reverse(Acc);
decode_put_expected([Field|Rest], Acc) ->
    {FieldName, [{<<"Exists">>, ExpectedData}, {<<"Value">>, [{FieldType, FieldValue}]}]} = Field,
    Expected = case ExpectedData of
        <<"true">> -> true;
        _ -> false
    end,
    decode_put_expected(Rest, [{FieldName, [{<<"Exists">>, Expected}, {FieldType, FieldValue}]}|Acc]).


decode_put_item([{}], Acc) ->
    [{}];
decode_put_item([], Acc) ->
    lists:reverse(Acc);
decode_put_item([{FieldName, [{FieldType, FieldValue}]}|Rest], Acc) ->
    decode_put_item(Rest, [{FieldName, {FieldType, FieldValue}}|Acc]).


decode_table_attributes([], Acc) ->
    Acc;
decode_table_attributes([Attribute|Rest], Acc) ->
    decode_table_attributes(Rest, [{<<"AttributeName">>, kvc:path("AttributeName", Attribute)},
                                   {<<"AttributeType">>, kvc:path("AttributeType", Attribute)} | Acc]).

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
    Acc;
decode_2i_key_schema([Attribute|Rest], Acc) ->
    decode_2i_key_schema(Rest, [{<<"AttributeName">>, kvc:path("AttributeName", Attribute)},
                                {<<"KeyType">>, kvc:path("KeyType", Attribute)} | Acc]).

decode_key_conditions([], Acc) ->
    lists:reverse(Acc);
decode_key_conditions([Condition|Rest], Acc) ->
    {Key, {struct, Args}} = Condition,
    AttributeValues = decode_attribute_values(kvc:path("AttributeValueList", Args), []),
    ComparisonOperator = kvc:path("ComparisonOperator", Args),
    decode_key_conditions(Rest, [{binary:bin_to_list(Key), AttributeValues, binary:bin_to_list(ComparisonOperator)}|Acc]).


decode_attribute_values([], Acc) ->
    lists:reverse(Acc);
decode_attribute_values([Attribute|Rest], Acc) ->
    {struct, [{Type, Value}]} = Attribute,
    ValueList = case is_list(Value) of
        true -> lists:map(fun(X) -> binary:bin_to_list(X) end, Value);
        _ -> binary:bin_to_list(Value)
    end,
    decode_attribute_values(Rest, [{binary:bin_to_list(Type), ValueList}|Acc]).


-ifdef(TEST).

decode_batch_get_item_test() ->
    Actual = decode_batch_get_item([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).

decode_batch_write_item_test() ->
    Actual = decode_batch_write_item([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).

decode_get_item_test() ->
    Json = "{
    \"AttributesToGet\": [
        \"string\"
    ],
    \"ConsistentRead\": \"boolean\",
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
    }",
    Response = mochijson2:decode(Json),
    Actual = decode_get_item(Response),
    io:format("~p", [Actual]),
    Expected = [{"AttributesToGet", ["string"]},
                {"ConsistentRead", "boolean"},
                {"Keys", [{"key1", {"B", "blob"}},
                            {"key2", {"BS", ["blob"]}},
                            {"key3", {"N", "string"}},
                            {"key4", {"NS", ["string"]}},
                            {"key5", {"S", "string"}},
                            {"key6", {"SS", ["string"]}}]},
                {"ReturnConsumedCapacity", "string"},
                {"TableName", "string"}],
    ?assertEqual(Expected, Actual).

encode_get_item_response_test() ->
     Data = {struct,[{<<"ConsumedCapacity">>,
                    {struct,[{<<"CapacityUnits">>,<<"number">>},
                             {<<"GlobalSecondaryIndexes">>,
                              {struct,[{<<"string">>,
                                        {struct,[{<<"CapacityUnits">>,
                                                  <<"number">>}]}}]}},
                             {<<"LocalSecondaryIndexes">>,
                              {struct,[{<<"string">>,
                                        {struct,[{<<"CapacityUnits">>,
                                                  <<"number">>}]}}]}},
                             {<<"Table">>,{struct,[{<<"CapacityUnits">>,<<"number">>}]}},
                             {<<"TableName">>,<<"string">>}]}},
                    {<<"Item">>,
                     {struct,[{<<"string">>,
                               {struct,[{<<"B">>,<<"blob">>},
                                        {<<"BS">>,[<<"blob">>]},
                                        {<<"N">>,<<"string">>},
                                        {<<"NS">>,[<<"string">>]},
                                        {<<"S">>,<<"string">>},
                                        {<<"SS">>,[<<"string">>]}]}}]}}]},
    Actual = encode_get_item_response(undefined),
    Expected = mochijson2:encode(Data),
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
        {item, [
            {<<"field1">>, {<<"B">>, <<"blob">>}},
            {<<"field2">>, {<<"BS">>, [<<"blob">>]}},
            {<<"field3">>, {<<"N">>, <<"string">>}},
            {<<"field4">>, {<<"NS">>, [<<"string">>]}},
            {<<"field5">>, {<<"S">>, <<"string">>}},
            {<<"field6">>, {<<"SS">>, [<<"string">>]}}
        ]},
        {return_consumed_capacity, <<"string">>},
        {return_item_collection_metrics, <<"string">>},
        {return_values, <<"string">>},
        {tablename, <<"table_name">>}],
    io:format("Actual: ~p~n", [Actual]),
    ?assertEqual(Expected, Actual).

encode_put_item_response_test() ->
    Data = {struct,[{<<"Attributes">>,
          {struct,[{<<"string">>,
                    {struct,[{<<"B">>,<<"blob">>},
                             {<<"BS">>,[<<"blob">>]},
                             {<<"N">>,<<"string">>},
                             {<<"NS">>,[<<"string">>]},
                             {<<"S">>,<<"string">>},
                             {<<"SS">>,[<<"string">>]}]}}]}},
         {<<"ConsumedCapacity">>,
          {struct,[{<<"CapacityUnits">>,<<"number">>},
                   {<<"GlobalSecondaryIndexes">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"CapacityUnits">>,
                                        <<"number">>}]}}]}},
                   {<<"LocalSecondaryIndexes">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"CapacityUnits">>,
                                        <<"number">>}]}}]}},
                   {<<"Table">>,{struct,[{<<"CapacityUnits">>,<<"number">>}]}},
                   {<<"TableName">>,<<"string">>}]}},
         {<<"ItemCollectionMetrics">>,
          {struct,[{<<"ItemCollectionKey">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"B">>,<<"blob">>},
                                       {<<"BS">>,[<<"blob">>]},
                                       {<<"N">>,<<"string">>},
                                       {<<"NS">>,[<<"string">>]},
                                       {<<"S">>,<<"string">>},
                                       {<<"SS">>,[<<"string">>]}]}}]}},
                   {<<"SizeEstimateRangeGB">>,[<<"number">>]}]}}]},
    Actual = encode_put_item_response(undefined),
    Expected = mochijson2:encode(Data),
    ?assertEqual(Expected, Actual).

decode_update_item_test() ->
    Actual = decode_update_item([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).

decode_delete_item_test() ->
    Actual = decode_delete_item([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).

decode_describe_table_test() ->
    Json_Bin = <<"{\"TableName\": \"table_name\"}">>,
    Actual = decode_describe_table(jsx:decode(Json_Bin)),
    io:format("Actual: ~p~n", [Actual]),
    Expected = [{tablename, <<"table_name">>}],
    ?assertEqual(Expected, Actual).

decode_create_table_test() ->
    Json_Bin = <<"{
        \"AttributeDefinitions\": [
            {
                \"AttributeName\": \"attr_name\",
                \"AttributeType\": \"attr_type\"
            }
        ],
        \"KeySchema\": [
            {
                \"AttributeName\": \"attr_name\",
                \"KeyType\": \"key_type\"
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
                {fields, [
                  {<<"AttributeName">>, <<"attr_name">>}, {<<"AttributeType">>, <<"attr_type">>}
                ]},
                {key_schema, [
                  {<<"AttributeName">>, <<"attr_name">>}, {<<"KeyType">>, <<"key_type">>}
                ]},
                {lsi, [[{<<"2i_name">>,
                        [{key_schema, [
                          {<<"AttributeName">>, <<"attr_name">>}, {<<"KeyType">>, <<"key_type">>}
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

decode_delete_table_test() ->
    Actual = decode_delete_table([]),
    Expected = ok,
    ?assertEqual(Expected, Actual).


% decode_query_test() ->
%     Json = "{
%         \"AttributesToGet\": [
%             \"string\"
%         ],
%         \"ConsistentRead\": \"boolean\",
%         \"ExclusiveStartKey\":
%             {
%                 \"Key1\": {\"B\": \"blob\"},
%                 \"Key2\": {\"BS\": [\"blob\"]},
%                 \"Key3\": {\"N\": \"string\"},
%                 \"Key4\": {\"NS\": [\"string\"]},
%                 \"Key5\": {\"S\": \"string\"},
%                 \"Key6\": {\"SS\": [\"string\"]}
%             },
%         \"IndexName\": \"string\",
%         \"KeyConditions\":
%             {
%                 \"Key1\": {
%                     \"AttributeValueList\": [{\"B\": \"blob\"}],
%                     \"ComparisonOperator\": \"string\"},
%                 \"Key2\": {
%                     \"AttributeValueList\": [{\"BS\": [\"blob\"]}],
%                     \"ComparisonOperator\": \"string\"},
%                 \"Key3\": {
%                     \"AttributeValueList\": [{\"N\": \"string\"}],
%                     \"ComparisonOperator\": \"string\"},
%                 \"Key4\": {
%                     \"AttributeValueList\": [{\"NS\": [\"string\"]}],
%                     \"ComparisonOperator\": \"string\"},
%                 \"Key5\": {
%                     \"AttributeValueList\": [{\"S\": \"string\"}],
%                     \"ComparisonOperator\": \"string\"},
%                 \"Key6\": {
%                     \"AttributeValueList\": [{\"SS\": [\"string\"]}],
%                     \"ComparisonOperator\": \"string\"},
%             },
%         \"Limit\": \"number\",
%         \"ReturnConsumedCapacity\": \"string\",
%         \"ScanIndexForward\": \"boolean\",
%         \"Select\": \"string\",
%         \"TableName\": \"string\"
%         }",
%     Actual = decode_query(mochijson2:decode(Json)),
%     Expected = [{"AttributesToGet", ["string"]},
%                 {"ConsistentRead", "boolean"},
%                 {"ExclusiveStartKey", [{"Key1", {"B", "blob"}},
%                                        {"Key2", {"BS", ["blob"]}},
%                                        {"Key3", {"N", "string"}},
%                                        {"Key4", {"NS", ["string"]}},
%                                        {"Key5", {"S", "string"}},
%                                        {"Key6", {"SS", ["string"]}}]},
%                 {"IndexName", "string"},
%                 {"KeyConditions", [
%                     {"Key1", [{"B", "blob"}], "string"},
%                     {"Key2", [{"BS", ["blob"]}], "string"},
%                     {"Key3", [{"N", "string"}], "string"},
%                     {"Key4", [{"NS", ["string"]}], "string"},
%                     {"Key5", [{"S", "string"}], "string"},
%                     {"Key6", [{"SS", ["string"]}], "string"}
%                      ]},
%                 {"Limit", "number"},
%                 {"ReturnConsumedCapacity", "string"},
%                 {"ScanIndexForward", "boolean"},
%                 {"Select", "string"},
%                 {"TableName", "string"}],
%     io:format("Actual: ~p", [Actual]),
%     ?assertEqual(Expected, Actual).

encode_query_response_test() ->
    Data = {struct,[{<<"ConsumedCapacity">>,
          {struct,[{<<"CapacityUnits">>,<<"number">>},
                   {<<"GlobalSecondaryIndexes">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"CapacityUnits">>,
                                        <<"number">>}]}}]}},
                   {<<"LocalSecondaryIndexes">>,
                    {struct,[{<<"string">>,
                              {struct,[{<<"CapacityUnits">>,
                                        <<"number">>}]}}]}},
                   {<<"Table">>,{struct,[{<<"CapacityUnits">>,<<"number">>}]}},
                   {<<"TableName">>,<<"string">>}]}},
         {<<"Count">>,<<"number">>},
         {<<"Items">>,
          [{struct,[{<<"string">>,
                     {struct,[{<<"B">>,<<"blob">>},
                              {<<"BS">>,[<<"blob">>]},
                              {<<"N">>,<<"string">>},
                              {<<"NS">>,[<<"string">>]},
                              {<<"S">>,<<"string">>},
                              {<<"SS">>,[<<"string">>]}]}}]}]},
         {<<"LastEvaluatedKey">>,
          {struct,[{<<"string">>,
                    {struct,[{<<"B">>,<<"blob">>},
                             {<<"BS">>,[<<"blob">>]},
                             {<<"N">>,<<"string">>},
                             {<<"NS">>,[<<"string">>]},
                             {<<"S">>,<<"string">>},
                             {<<"SS">>,[<<"string">>]}]}}]}}]},
    Expected = mochijson2:encode(Data),
    Actual = encode_query_response(undefined),
    ?assertEqual(Expected, Actual).


decode_scan_test() ->
    Expected = ok,
    Actual = decode_scan(undefined),
    ?assertEqual(Expected, Actual).

-endif.
