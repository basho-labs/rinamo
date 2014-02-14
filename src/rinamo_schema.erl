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

-module(rinamo_schema).

-export([dynamo_to_solr/1, solr_to_dynamo/1]).

-include_lib("rinamo_schema.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


dynamo_to_solr(DynamoSchema) ->
	BucketName = proplists:get_value("TableName", DynamoSchema),

	DynamoFields = process_dynamo_attributes(DynamoSchema),
	
	io:format("DynamoFields: ~p", [DynamoFields]),
	
	XmlFields = lists:map(fun(X) -> dynamo_field_to_solr(X) end, DynamoFields),

	{SolrFields, CopyFields} = lists:foldl(
		fun({FieldDef, CopyFieldDef}, {FieldDefs, CopyFieldDefs}) -> 
			{FieldDef ++ FieldDefs, CopyFieldDef ++ CopyFieldDefs} end, 
		{[], []}, 
		XmlFields), 

	% store Dynamo schema for describe operation
	RawSchema = proplists:get_value("RawSchema", DynamoSchema),

	XML = lists:flatten(
        ?DEFAULT_SCHEMA_HEADER ++
        xmerl:export_simple_content(SolrFields,xmerl_xml) ++
        ?DEFAULT_SCHEMA_MIDDLE ++
        xmerl:export_simple_content(CopyFields,xmerl_xml) ++
        ?DEFAULT_SCHEMA_FOOTER),

	{BucketName, XML, RawSchema}.

solr_to_dynamo(_SolrSchema) ->
	% retrieve raw schema
	ok.


%% Internal Functions %%

dynamo_field_to_solr({Name, DynamoType, Indexed, Copied}) ->
	{Type, MultiValued} = proplists:get_value(DynamoType, ?TYPE_MAP, {"string", false}),
	FieldDef = [{field, [{name, Name}, 
						 {type, Type}, 
						 {indexed, Indexed}, 
						 {stored, Copied},
						 {multiValued, MultiValued}],[]}],
    case Copied of
    	true -> {FieldDef, [{copyField, [{source, Name}, {dest, "text"}],[]}],[]};
    	_ -> {FieldDef,[]}
    end.

process_dynamo_attributes(DynamoSchema) ->
	{LocalSecondaryIndexes, LocalProjectedValues} = extract_2i(DynamoSchema),

	PrimaryKeys = lists:map(
			fun(X) -> process_attribute(X, true, LocalProjectedValues) end, 
			proplists:get_value("KeySchema", DynamoSchema)),
	LocalSecondaryIndexKeySchema = lists:map(
			fun(X) -> process_attribute(X, true, LocalProjectedValues) end, 
			LocalSecondaryIndexes),
	Fields = lists:map(
			fun(X) -> process_attribute(X, true, LocalProjectedValues) end, 
			proplists:get_value("Fields", DynamoSchema)),
	
	Fields ++ PrimaryKeys ++ LocalSecondaryIndexKeySchema.

process_attribute({Name, Type}, Indexed, LocalProjectedValues) ->
	{Name, Type, Indexed, lists:member(Name, LocalProjectedValues)}.

extract_2i(DynamoSchema) ->
	case proplists:get_value("LocalSecondaryIndexes", DynamoSchema, undefined) of
		undefined -> {[], []};
		Value -> extract_2i(Value, [], [])
	end.
extract_2i([], KeySchema, Projection) ->
	{KeySchema, Projection};
extract_2i([[{_IndexName, Index}]|Rest], KeySchema, Projection) ->
	Fields = proplists:get_value("KeySchema", Index),
	ProjectedFields = case proplists:get_value("Projection", Index, undefined) of
		[{"NonKeyAttributes", NonKeyAttributes}, {_,_}] -> io:format("~p", [NonKeyAttributes]), NonKeyAttributes;
		_ -> []
	end,
	extract_2i(Rest, KeySchema ++ Fields, Projection ++ ProjectedFields).




-ifdef(TEST).

dynamo_to_solr_test() ->
	Data = [{"TableName", "table_name"},
			{"Fields", [{"attr_name1", "S"}]},
			{"KeySchema", [{"attr_name2", "S"}]},
			{"LocalSecondaryIndexes", [[{"2i_name", 
										[{"KeySchema", [{"attr_name3", "S"}]},
										 {"Projection", [{"NonKeyAttributes", ["attr_name"]},
										 				 {"ProjectionType", "projection_type"}]}]
						 			}]]},
			{"ProvisionedThroughput", [{"ReadCapacityUnits", "number"},
									   {"WriteCapacityUnits", "number"}]}
			],
	{_BucketName, XML, _RawSchema} = dynamo_to_solr(Data),
	io:format("Actual: ~p", [XML]),
	?assert(length(XML) > 0).
	

extract_2i_test() ->
	TwoEye = [{"LocalSecondaryIndexes", [[{"2i_name", 
										  [{"KeySchema", [{"attr_name", "key_type"}]},
										   {"Projection", [{"NonKeyAttributes", ["attr_name"]},
								 		  				 {"ProjectionType", "projection_type"}]}]
							    		}]]}],
	Actual = extract_2i(TwoEye),
	Expected = {[{"attr_name", "key_type"}],
				["attr_name"]},
	?assertEqual(Expected, Actual).

process_dynamo_attributes_test() ->
	Data = [{"TableName", "table_name"},
			{"Fields", [{"attr_name1", "attr_type1"}]},
			{"KeySchema", [{"attr_name2", "key_type2"}]},
			{"LocalSecondaryIndexes", [[{"2i_name", 
										[{"KeySchema", [{"attr_name3", "key_type3"}]},
										 {"Projection", [{"NonKeyAttributes", ["attr_name"]},
										 				 {"ProjectionType", "projection_type"}]}]
						 			}]]},
			{"ProvisionedThroughput", [{"ReadCapacityUnits", "number"},
									   {"WriteCapacityUnits", "number"}]}
			],
	Actual = process_dynamo_attributes(Data),
	Expected = [{"attr_name1", "attr_type1", true, false},
				{"attr_name2", "key_type2", true, false},
				{"attr_name3", "key_type3", true, false}],
	?assertEqual(Expected, Actual).

-endif.
