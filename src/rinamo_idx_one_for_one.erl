-module(rinamo_idx_one_for_one).

-export([
    store/4,
    query/4]).

-include("rinamo.hrl").

store(PartitionNS, PartitionId, Value, Item) ->
    % track the Item's dispersion via a dv_orswot
    RB = erlang:iolist_to_binary([<<"Rinamo">>, ?RINAMO_SEPARATOR, <<"Index">>]),
    RK = erlang:iolist_to_binary([
        PartitionNS, ?RINAMO_SEPARATOR,
        PartitionId, ?RINAMO_SEPARATOR,
        <<"RefList">>]),
    RV = Value,
    _ = rinamo_set:add(rinamo_set:client(), RB, RK, RV),

    % store Item as a dispersed object in Riak
    % logic for a get has to consider this may not succeed
    B = PartitionNS,
    K = erlang:iolist_to_binary([PartitionId, ?RINAMO_SEPARATOR, Value]),
    V = jsx:encode(Item),
    _ = rinamo_kv:put(rinamo_kv:client(), B, K, V, "application/json"),

    ok.

query(PartitionNS, PartitionId, Query, Conditions) ->
    {Attribute, Operands, Operator} = Query,
    RB = erlang:iolist_to_binary([<<"Rinamo">>, ?RINAMO_SEPARATOR, <<"Index">>]),
    RK = erlang:iolist_to_binary([
        PartitionNS, ?RINAMO_SEPARATOR,
        PartitionId, ?RINAMO_SEPARATOR,
        <<"RefList">>]),
    {value, RefList} = rinamo_set:value(rinamo_set:client(), RB, RK),

    lager:debug("Attr, Operands, Operator: [~p, ~p, ~p]~n", [Attribute, Operands, Operator]),
    lager:debug("Conditions: ~p~n", [Conditions]),

    PreFilteredList = pre_filter(Operands, Operator, RefList),

    ItemAttrList = fetch_items(PartitionNS, PartitionId, PreFilteredList, []),

    post_filter(ItemAttrList, Conditions).

delete(I, Do, Not, Know, Yet) ->
    ok.

%% Internal
pre_filter(Operands, Operator, List) ->
    case Operands of
        [{OpType, OpVal}] ->
            pre_filter(OpType, OpVal, Operator, List);
        [{BOpType, BOpVal}, {EOpType, EOpVal}] ->
            pre_filter(BOpType, BOpVal, EOpType, EOpVal, Operator, List);
        _ ->
            throw(validation_operand_count)
    end.

pre_filter(_, OpVal, Operator, List) ->
    lists:filter(fun(X) -> eval(X, OpVal, Operator) end, List).

pre_filter(_, BeginVal, _, EndVal, Operator, List) ->
    lists:filter(fun(X) -> eval(X, BeginVal, EndVal, Operator) end, List).

post_filter(ItemAttrList, Conditions) ->
    lists:foldl(
        fun(Condition, ItemAttrList) ->
            case Condition of
            {C_Attr, [{C_OpType, C_OpVal}], C_Op} ->
                lists:filter(fun(ItemAttrs) ->
                    case kvc:path(C_Attr, ItemAttrs) of
                        [{Attr_Type, Attr_Val}] ->
                            (Attr_Type =:= C_OpType) and eval(Attr_Val, C_OpVal, C_Op);
                        [] -> false;
                        _ -> false
                    end
                end, ItemAttrList);
            {C_Attr, [{BOpType, BOpVal}, {EOpType, EOpVal}], C_Op} ->
                lists:filter(fun(ItemAttrs) ->
                    case kvc:path(C_Attr, ItemAttrs) of
                        [{Attr_Type, Attr_Val}] ->
                            (Attr_Type =:= BOpType) and
                            (Attr_Type =:= EOpType) and
                            eval(Attr_Val, BOpVal, EOpVal, C_Op);
                        [] -> false;
                        _ -> false
                    end
                end, ItemAttrList);
            _ ->
                throw(validation_operand_count)
            end
        end, ItemAttrList, Conditions).

% TODO:  limit fetch to 1 MB; return LastEvaluatedKey
fetch_items(_, _, [], Acc) ->
    lists:reverse(Acc);
fetch_items(B, PartitionId, [Ref|Rest], Acc) ->
    K = erlang:iolist_to_binary([PartitionId, ?RINAMO_SEPARATOR, Ref]),
    case rinamo_kv:get(rinamo_kv:client(), B, K) of
        {value, JSON} ->
            Item = jsx:decode(JSON),
            fetch_items(B, PartitionId, Rest, [Item | Acc]);
        {error, notfound} ->
            % either we are partitioned, or the index is dirty
            % (we can't tell which)
            fetch_items(B, PartitionId, Rest, Acc);
        _ ->
            % what else can go wrong?
            fetch_items(B, PartitionId, Rest, Acc)
    end.

eval(X, Y, Operator) ->
    case Operator of
        <<"EQ">> ->
            X =:= Y;
        <<"LE">> ->
            X =< Y;
        <<"LT">> ->
            X < Y;
        <<"GE">> ->
            X >= Y;
        <<"GT">> ->
            X > Y;
        <<"BEGINS_WITH">> ->
            Len = size(Y),
            case size(X) >= Len of
                true ->
                    <<Prefix:Len/binary, _/binary>> = X,
                    eval(Prefix, Y, <<"EQ">>);
                false -> false
            end;
        _ ->
            throw(validation_comparison_type)
    end.

eval(X, Y1, Y2, Operator) ->
    case Operator of
        <<"BETWEEN">> ->
            (X >= Y1) and (X =< Y2);
        _ ->
            throw(validation_comparison_type)
    end.
