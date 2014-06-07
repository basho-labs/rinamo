%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2014 Basho Technologies, Inc.  All Rights Reserved.
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
%% ---------------------------------------------------------------------

-module(rinamo_results).

-export([filter/2,
         eval/3,
         eval/4]).

filter(ItemAttrList, Conditions) ->
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
