%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.
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
%% -------------------------------------------------------------------

-module(rinamo_query).

-export([dynamo_get_to_solr/1,
         dynamo_query_to_solr/1,
         dynamo_scan_to_solr/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

dynamo_get_to_solr({BucketName, DynamoGet, _Attributes}) ->
    %% Doesn't handle gets for multivalues
    FieldQueries = lists:map(fun({FieldName, {_Type, Value}}) -> eq(FieldName, Value) end, DynamoGet),
    [{"q", string:join(FieldQueries, " AND ")}] ++ default_solr_params().

dynamo_query_to_solr(DynamoQuery) ->
    KeyConditions = proplists:get_value("KeyConditions", DynamoQuery),
    Query = key_conditions_to_query(KeyConditions, []),
    [{"q", string:join(Query, " AND ")}] ++ default_solr_params().

dynamo_scan_to_solr(DynamoScan) ->
    ok.

%% internal

default_solr_params() ->
    [
        {"omitHeader", true},
        {"wt", "json"},
        {"fl", "_yz_rb,_yz_rk"}
    ].

token_to_string(V) when is_binary(V) ->
    binary_to_list(V);
token_to_string(V) when not is_binary(V) ->
    lists:flatten(io_lib:format("~p", [V])).

token_to_number(Value) ->
    %% todo: log conversion error
    case string:chr(Value, $.) of
        0 -> {Int, _Rem} = string:to_integer(Value),
             Int;
        _ -> {Float, _Rem} = string:to_float(Value),
             Float
    end.

eq(FieldName, Value) ->
    FieldName ++ ":" ++ Value.

gt(FieldName, Value) ->
    Number = token_to_number(Value),
    io:format("number: ~p", [Number]),
    FieldName ++ ":" ++ "[" ++ token_to_string(Number + 1) ++ " TO *]".

gte(FieldName, Value) ->
    FieldName ++ ":" ++ "[" ++ Value ++ " TO *]".

lt(FieldName, Value) ->
    Number = token_to_number(Value),
    FieldName ++ ":" ++ "[* TO " ++ token_to_string(Number - 1) ++ "]".

lte(FieldName, Value) ->
    Number = token_to_number(Value),
    FieldName ++ ":" ++ "[* TO " ++ Value ++ "]".

between(FieldName, [From, To]) ->
    FieldName ++ ":" ++ "[" ++ From ++ " TO " ++ To ++ "]".

begins_with(FieldName, ValuePrefix) ->
    FieldName ++ ":" ++ ValuePrefix ++ "*".

key_conditions_to_query([], Acc) ->
    lists:reverse(Acc);
key_conditions_to_query([{Key, ValueArray, Operator}|Rest], Acc) ->
    Values = lists:map(fun({_Type, Value}) -> Value end, ValueArray),

    ValueArg = case length(Values) of
        1 -> lists:nth(1, Values);
        _ -> Values
    end,

    Query = case Operator of
        "EQ" -> eq(Key, ValueArg);
        "LE" -> lte(Key, ValueArg);
        "LT" -> lt(Key, ValueArg);
        "GE" -> gte(Key, ValueArg);
        "GT" -> gt(Key, ValueArg);
        "BEGINS_WITH" -> begins_with(Key, ValueArg);
        "BETWEEN" -> between(Key, ValueArg);
        _ -> eq(Key, ValueArg)
    end,

    key_conditions_to_query(Rest, [Query|Acc]).


-ifdef(TEST).
eq_test() ->
    Expected = "Field:Value",
    Actual = eq("Field", "Value"),
    ?assertEqual(Expected, Actual).

gt_test() ->
    Expected = "Field:[6 TO *]",
    Actual = gt("Field", "5"),
    ?assertEqual(Expected, Actual).

gte_test() ->
    Expected = "Field:[5 TO *]",
    Actual = gte("Field", "5"),
    ?assertEqual(Expected, Actual).

lt_test() ->
    Expected = "Field:[* TO 4]",
    Actual = lt("Field", "5"),
    ?assertEqual(Expected, Actual).

lte_test() ->
    Expected = "Field:[* TO 5]",
    Actual = lte("Field", "5"),
    ?assertEqual(Expected, Actual).

between_test() ->
    Expected = "Field:[0 TO 10]",
    Actual = between("Field", ["0", "10"]),
    ?assertEqual(Expected, Actual).

begins_with_Test() ->
    Expected = "Field:Valu*",
    Actual = begins_with("Field", "Valu"),
    ?assertEqual(Expected, Actual).

dynamo_get_to_solr_test() ->
    Items = [{"field1", {"B", "blob"}},
             {"field2", {"N", "number"}},
             {"field3", {"S", "string"}}
            ],
    Data = {"BucketName", Items, ["field1", "field2", "field3"]},
    Expected = [{"q","field1:blob AND field2:number AND field3:string"},
                {"omitHeader", true},
                {"wt", "json"},
                {"fl", "_yz_rb,_yz_rk"}],
    Actual = dynamo_get_to_solr(Data),
    ?assertEqual(Expected, Actual).

dynamo_query_to_solr_test() ->
    KeyConditions = [
        {"Key1", [{"S", "Value1"}], "EQ"},
        {"Key2", [{"N", "0"}], "GT"},
        {"Key3", [{"N", "0"}], "GE"},
        {"Key4", [{"N", "10"}], "LT"},
        {"Key5", [{"N", "10"}], "LE"},
        {"Key6", [{"N", "0"}, {"N", "10"}], "BETWEEN"},
        {"Key7", [{"S", "Valu"}], "BEGINS_WITH"}],
    DynamoQuery = [{"KeyConditions", KeyConditions},
                   {"TableName", "TestTable"}],
    Expected = [{"q", "Key1:Value1 AND Key2:[1 TO *] AND Key3:[0 TO *] AND Key4:[* TO 9] AND Key5:[* TO 10] AND Key6:[0 TO 10] AND Key7:Valu*"},
                {"omitHeader", true},
                {"wt", "json"},
                {"fl", "_yz_rb,_yz_rk"}],
    Actual = dynamo_query_to_solr(DynamoQuery),
    ?assertEqual(Expected, Actual).

dynamo_scan_to_solr_test() ->
    Expected = ok,
    Actual = dynamo_scan_to_solr(undefined),
    ?assertEqual(Expected, Actual).

key_conditions_to_query_test() ->
    Expected = ["Key1:Value1", "Key2:[1 TO *]", "Key3:[0 TO *]", "Key4:[* TO 9]", "Key5:[* TO 10]", "Key6:[0 TO 10]", "Key7:Valu*"],
    KeyConditions = [
        {"Key1", [{"S", "Value1"}], "EQ"},
        {"Key2", [{"N", "0"}], "GT"},
        {"Key3", [{"N", "0"}], "GE"},
        {"Key4", [{"N", "10"}], "LT"},
        {"Key5", [{"N", "10"}], "LE"},
        {"Key6", [{"N", "0"}, {"N", "10"}], "BETWEEN"},
        {"Key7", [{"S", "Valu"}], "BEGINS_WITH"}],
    Actual = key_conditions_to_query(KeyConditions, []),
    ?assertEqual(Expected, Actual).

-endif.