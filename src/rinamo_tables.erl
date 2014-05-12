-module(rinamo_tables).

-export([create_table/3, list_tables/1,
         load_table_def/2, delete_table/2]).

-include("rinamo.hrl").

create_table(Table, RawSchema, AWSContext) ->
    lager:debug("RawSchema: ~p~n", [RawSchema]),

    UserKey = AWSContext#state.user_key,

    B = UserKey,
    List_K = <<"TableList">>,
    Table_K = Table,
    Table_V = jsx:encode(RawSchema),

    R0 = rinamo_crdt_set:add(rinamo_crdt_set:client(), B, List_K, Table),

    R1 = rinamo_kv:put(rinamo_kv:client(), B, Table_K, Table_V, "application/json"),

    lager:debug("Result: [~p, ~p]~n", [R0, R1]),

    {R0, R1}.

list_tables(AWSContext) ->
    UserKey = AWSContext#state.user_key,

    B = UserKey,
    List_K = <<"TableList">>,

    case rinamo_crdt_set:value(rinamo_crdt_set:client(), B, List_K) of
        notfound -> [];
        {value, List} -> List
    end.

load_table_def(Table, AWSContext) ->
    UserKey = AWSContext#state.user_key,

    B = UserKey,
    Table_K = Table,

    {_, Table_V} = rinamo_kv:get(rinamo_kv:client(), B, Table_K),

    case Table_V of
        {insufficient_vnodes, _, _, _} -> throw(insufficient_vnodes_available);
        notfound -> notfound;
        _ -> jsx:decode(Table_V)
    end.

delete_table(Table, AWSContext) ->
    TD = load_table_def(Table, AWSContext),
    case TD of
        notfound -> throw(table_missing);
        _ -> ok
    end,

    UserKey = AWSContext#state.user_key,

    B = UserKey,
    Table_K = Table,

    % TODO: the following can be async

    _ = rinamo_kv:delete(rinamo_kv:client(), B, Table_K),

    List_K = <<"TableList">>,
    _ = rinamo_crdt_set:remove(rinamo_crdt_set:client(), B, List_K, Table),

    TD.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

create_table_test() ->
    meck:new([rinamo_kv, rinamo_crdt_set], [non_strict]),
    meck:expect(rinamo_kv, client, 0, ok),
    meck:expect(rinamo_crdt_set, client, 0, ok),
    meck:expect(rinamo_kv, put, 5, ok),
    meck:expect(rinamo_crdt_set, add, 4, ok),

    AWSContext=#state{ user_key = <<"TEST_API_KEY">> },
    Table = <<"TableName">>,
    Fields = [{<<"AttributeName">>, <<"attr_name">>}, {<<"AttributeType">>, <<"attr_type">>}],
    KeySchema = [{<<"AttributeName">>, <<"attr_name">>}, {<<"KeyType">>, <<"key_type">>}],
    LSI = [{"2i_name", [{<<"KeySchema">>, [{<<"AttributeName">>, <<"lsi_attr_name">>},
                                           {<<"KeyType">>, <<"lsi_key_type">>}]},
                        {<<"Projection">>,[{<<"NonKeyAttributes">>, [<<"attr_name">>]},
                                           {<<"ProjectionType">>, <<"projection_type">>}]}]}],
    ProvisionedThroughput = [{<<"ReadCapacityUnits">>, 10}, {<<"WriteCapacityUnits">>, 2}],

    Actual = create_table(Table, '{"raw":"schema"}', AWSContext),
    Expected = {ok, ok},
    ?assertEqual(Expected, Actual),

    meck:unload([rinamo_kv, rinamo_crdt_set]).


list_tables_test() ->
    meck:new(rinamo_crdt_set, [non_strict]),
    meck:expect(rinamo_crdt_set, client, 0, ok),
    meck:expect(rinamo_crdt_set, value, 3, {value, [<<"one">>, <<"two">>, <<"three">>]}),

    AWSContext=#state{ user_key = <<"TEST_API_KEY">> },

    Actual = list_tables(AWSContext),
    Expected = [<<"one">>, <<"two">>, <<"three">>],
    ?assertEqual(Expected, Actual),

    meck:unload(rinamo_crdt_set).

load_table_def_test() ->
    meck:new(rinamo_kv, [non_strict]),
    meck:expect(rinamo_kv, client, 0, ok),
    meck:expect(rinamo_kv, get, 3, {value, <<"[\"Some_Table_Def_JSON_Here\"]">>}),

    Table = <<"Some_Table">>,
    AWSContext=#state{ user_key = <<"TEST_API_KEY">> },

    Actual = load_table_def(Table, AWSContext),
    Expected = [<<"Some_Table_Def_JSON_Here">>],
    ?assertEqual(Expected, Actual),

    meck:unload(rinamo_kv).

delete_table_test() ->
    meck:new([rinamo_kv, rinamo_crdt_set], [non_strict]),
    meck:expect(rinamo_kv, client, 0, ok),
    meck:expect(rinamo_kv, get, 3, {value, <<"[\"Some_Table_Def_JSON_Here\"]">>}),
    meck:expect(rinamo_kv, delete, 3, ok),
    meck:expect(rinamo_crdt_set, client, 0, ok),
    meck:expect(rinamo_crdt_set, remove, 4, ok),

    Table = <<"Another Table">>,
    AWSContext=#state{ user_key = <<"TEST_API_KEY">> },

    Actual = delete_table(Table, AWSContext),
    Expected = [<<"Some_Table_Def_JSON_Here">>],
    ?assertEqual(Expected, Actual),

    meck:unload([rinamo_kv, rinamo_crdt_set]).

-endif.
