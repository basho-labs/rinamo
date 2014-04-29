-module(rinamo_items).

-export([
    put_item/3,
    get_item/3,
    delete_item/3,
    query/3]).

-include("rinamo.hrl").

-spec put_item(binary(), any(), #state{ user_key :: binary() }) -> ok.
put_item(Table, Item, AWSContext) ->
    UserKey = AWSContext#state.user_key,
    case get_keyschema(Table, AWSContext) of
        [{hash, HashKeyAttribute}] ->
            [{HashFieldType, HashKeyValue}] = kvc:path(HashKeyAttribute, Item),
            store_hash_key(UserKey, Table, HashKeyValue, Item);
        [{hash, HashKeyAttribute}, {range, RangeKeyAttribute}] ->
            [{HashFieldType, HashKeyValue}] = kvc:path(HashKeyAttribute, Item),
            [{RangeFieldType, RangeKeyValue}] = kvc:path(RangeKeyAttribute, Item),
            store_range_key(UserKey, Table, RangeKeyAttribute, HashKeyValue, RangeKeyValue, Item)
    end.

-spec get_item(binary(), binary(), #state{ user_key :: binary() }) -> any().
get_item(Table, Key, AWSContext) ->
    UserKey = AWSContext#state.user_key,
    B = erlang:iolist_to_binary([UserKey, ?RINAMO_SEPARATOR, Table]),

    {_, Item_V} = rinamo_kv:get(rinamo_kv:client(), B, Key),

    case Item_V of
        {insufficient_vnodes, _, _, _} -> throw(insufficient_vnodes_available);
        notfound -> notfound;
        _ -> jsx:decode(Item_V)
    end.

-spec delete_item(binary(), binary(), #state{ user_key :: binary() }) -> ok.
delete_item(Table, Key, AWSContext) ->
    UserKey = AWSContext#state.user_key,
    _ = case Key of
        [{_, {_, HashKeyVal}}] ->
            B = erlang:iolist_to_binary([UserKey, ?RINAMO_SEPARATOR, Table]),
            rinamo_kv:delete(rinamo_kv:client(), B, HashKeyVal);
        [{_, {_, HashKeyVal}}, {RangeKeyAttribute, {_, RangeKeyVale}}] ->
            % ------- begin index concern
            % ------- end index concern
            ok
    end,

    ok.

query(Table, KeyConditions, AWSContext) ->
    UserKey = AWSContext#state.user_key,

    % TODO: more validation
    %  - Value must not be empty string
    %  - HashKeyOperator should always be "EQ".
    %  - Should be at least one key condition matching key schema.
    %    - detected within map key conditions
    %  - Error if range condition not specified for table using range key
    %    - if specified, condition type should match schema type
    %    - if not a range table, ok - but have to handle that case specifically

    [{hash, {HashKeyAttr, [{HashKeyType, HashKeyValue}], HashKeyOperator}},
     {range, {RangeKeyAttr, RangeKeyOperands, RangeKeyOperator}},
     {remaining, PostFilterConditions}] = map_key_conditions(Table, KeyConditions, AWSContext),

    lager:debug("Hash Key Details: ~p,~p,~p,~p~n", [HashKeyAttr, HashKeyType, HashKeyValue, HashKeyOperator]),
    lager:debug("Range Key Details: ~p,~p,~p,~p~n", [RangeKeyAttr, RangeKeyOperands, RangeKeyOperator]),
    lager:debug("Post Filter Details: ~p~n", [PostFilterConditions]),

    % ------- begin index concern

    M = rinamo_config:get_index_strategy(),
    F = query,
    A = [
        erlang:iolist_to_binary([UserKey, ?RINAMO_SEPARATOR, Table, ?RINAMO_SEPARATOR, RangeKeyAttr]),
        HashKeyValue,
        {RangeKeyAttr, RangeKeyOperands, RangeKeyOperator},
        PostFilterConditions
    ],
    lager:debug("Index Strat: ~p~n", [M]),
    erlang:apply(M, F, A).

    % ------- end index concern

%% Internal

-spec store_hash_key(binary(), binary(), [binary()], binary()) -> ok.
store_hash_key(User, Table, [], Item) ->
    ok;
store_hash_key(User, Table, [Key|Rest], Item) ->
    store_hash_key(User, Table, Rest, Item),
    store_hash_key(User, Table, Key, Item);
store_hash_key(User, Table, Key, Item) ->
    lager:debug("Storing as Hash Key: ~p:", [Key]),

    B = erlang:iolist_to_binary([User, ?RINAMO_SEPARATOR, Table]),
    Value = jsx:encode(Item),

    _ = rinamo_kv:put(rinamo_kv:client(), B, Key, Value, "application/json"),
    ok.

store_range_key(UserKey, Table, RangeKeyAttribute, HashKeyValue, RangeKeyValue, Item) ->
    lager:debug("Storing as Range Key: ~p::~p:", [HashKeyValue, RangeKeyValue]),

    % ------- begin index concern

    StrategyModule = rinamo_config:get_index_strategy(),
    M = rinamo_config:get_index_strategy(),
    F = store,
    A = [
        erlang:iolist_to_binary([UserKey, ?RINAMO_SEPARATOR, Table, ?RINAMO_SEPARATOR, RangeKeyAttribute]),
        HashKeyValue,
        RangeKeyValue,
        jsx:encode(Item)
    ],
    Result = erlang:apply(M, F, A),

    % ------- end index concern

    ok.

% Fetches the primary key for a given table.  In DynamoDB, a primary key
% could be either just a Hash Key, or a composite of a Hash Key and Range key.
%
% result tuple array is sorted by key type so callers can pattern match on
% [{hash, HashAttr}] or [{hash, HashAttr}, {range, RangeAttr}]
-spec get_keyschema(binary(), #state{ user_key :: binary() }) -> [tuple()].
get_keyschema(Table, AWSContext) when is_binary(Table) ->
    TD = rinamo_tables:load_table_def(Table, AWSContext),
    case TD of
      notfound -> throw(table_missing);
      _ ->
          get_keyschema(kvc:path("KeySchema", TD), [])
    end;
get_keyschema([], Acc) ->
    % sort final list on key type, (make hash come before range)
    lists:keysort(1, Acc);
get_keyschema([Attribute|Rest], Acc) ->
    % handle cases where attribute type and key type are out of order within
    % a single key definition.
    % (attribute_name should come before key_type)
    OrderedAttr = lists:keysort(1, Attribute),
    [{_, AttributeName}, {_, KeyType}] = OrderedAttr,
    KeyTypeAtom = erlang:list_to_atom(string:to_lower(erlang:binary_to_list(KeyType))),
    get_keyschema(Rest, [{KeyTypeAtom, AttributeName} | Acc]).


% Determines which key conditions are hash or range based, and which are not.
% Returns a list of condition tuples where each tuple indicates if the
% condition is (by atom) hash|range|remaining.
map_key_conditions(Table, KeyConditions, AWSContext) when is_binary(Table) ->
    KeySchema = get_keyschema(Table, AWSContext),
    map_key_conditions(KeySchema, KeyConditions, []);
map_key_conditions([], LeftoverConditions, Acc) ->
    lists:keysort(1, [{remaining, LeftoverConditions} | Acc]);
map_key_conditions([KeyPart|Rest], KeyConditions, Acc) ->
    {KeyType, KeyAttr} = KeyPart,
    case lists:keytake(KeyAttr, 1, KeyConditions) of
        {value, MatchedCondition, NewKeyConditions} ->
            map_key_conditions(Rest, NewKeyConditions, [{KeyType, MatchedCondition} | Acc]);
        false ->
            % key schema not in the conditions (error)
            map_key_conditions(Rest, KeyConditions, Acc)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hash_table_fixture() ->
    {_, Fixture} = file:read_file("../tests/fixtures/hash_table.json"),
    Fixture.

range_table_fixture() ->
    {_, Fixture} = file:read_file("../tests/fixtures/range_table.json"),
    Fixture.

item_fixture() ->
    {_, Fixture} = file:read_file("../tests/fixtures/item.json"),
    Fixture.

put_item_test() ->
    meck:new([rinamo_tables, rinamo_kv], [non_strict]),
    meck:expect(rinamo_tables, load_table_def, 2, jsx:decode(hash_table_fixture())),
    meck:expect(rinamo_kv, client, 0, ok),
    meck:expect(rinamo_kv, put, 5, ok),

    Table = <<"TableName">>,
    Item = kvc:path("Item", jsx:decode(item_fixture())),
    AWSContext=#state{ user_key = <<"TEST_API_KEY">> },
    _ = put_item(Table, Item, AWSContext),

    meck:unload([rinamo_tables, rinamo_kv]).

get_item_test() ->
    meck:new(rinamo_kv, [non_strict]),
    meck:expect(rinamo_kv, client, 0, ok),
    meck:expect(rinamo_kv, get, 3, {value, <<"[\"Some_Item_Def_JSON_Here\"]">>}),

    Table = <<"Item Table">>,
    Key = <<"Some_Item_Key">>,
    AWSContext=#state{ user_key = <<"TEST_API_KEY">> },

    Actual = get_item(Table, Key, AWSContext),
    Expected = [<<"Some_Item_Def_JSON_Here">>],
    ?assertEqual(Expected, Actual),

    meck:unload(rinamo_kv).

delete_item_test() ->
    meck:new(rinamo_kv, [non_strict]),
    meck:expect(rinamo_kv, client, 0, ok),
    meck:expect(rinamo_kv, delete, 3, {value, <<"[\"Some_Item_Def_JSON_Here\"]">>}),

    Table = <<"Item Table">>,
    Keys = [
        {<<"HASH">>, {<<"S">>, <<"Some_Item_Key">>}}
    ],
    AWSContext=#state{ user_key = <<"TEST_API_KEY">> },

    Actual = delete_item(Table, Keys, AWSContext),
    Expected = ok,
    ?assertEqual(Expected, Actual),

    meck:unload(rinamo_kv).

map_key_condition_test() ->
    meck:new([rinamo_tables, rinamo_kv], [non_strict]),
    meck:expect(rinamo_tables, load_table_def, 2, jsx:decode(range_table_fixture())),
    meck:expect(rinamo_kv, client, 0, ok),

    Table = <<"TableName">>,
    KeyConditions = [
        {<<"Title">>,[{<<"S">>,<<"Some Title">>}],<<"EQ">>},
        {<<"ISBN">>, [{<<"N">>,<<"9876">>}],<<"GT">>},
        {<<"Id">>,[{<<"N">>,<<"101">>}],<<"EQ">>},
        {<<"AnotherCondition">>, [{<<"S">>,<<"XYZ">>}],<<"LT">>}],
    AWSContext=#state{ user_key = <<"TEST_API_KEY">> },

    Actual = map_key_conditions(Table, KeyConditions, AWSContext),

    Expected = [
     {hash,{<<"Id">>,[{<<"N">>,<<"101">>}],<<"EQ">>}},
     {range,{<<"Title">>,[{<<"S">>,<<"Some Title">>}],<<"EQ">>}},
     {remaining,[{<<"ISBN">>,[{<<"N">>,<<"9876">>}],<<"GT">>},
                 {<<"AnotherCondition">>,[{<<"S">>,<<"XYZ">>}],<<"LT">>}]}],

    ?assertEqual(Expected, Actual),

    meck:unload([rinamo_tables, rinamo_kv]).


-endif.
