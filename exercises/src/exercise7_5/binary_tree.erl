%% @author ivershinina
%% @doc @todo Add description to binary_tree.


-module(binary_tree).
-include("records.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([sum/1, max_val/1, is_binary_tree_ordered/1, insert_value_to_ordered_tree/2]).

-define(INFINITY, 1000000000).

sum(Tree) when is_record(Tree, binary_tree) ->
    get_sum(Tree, 0);
sum(Tree) when is_record(Tree, leaf) ->
    #leaf{value = Val} = Tree,
    Val;
sum(_Tree) ->
    {error, not_a_binary_tree}.

max_val(Tree) when is_record(Tree, binary_tree) ->
    case find_max_val(Tree) of
        {not_duplicated, Val} -> {ok, Val};
        {duplicated, _Val} -> {ok, there_is_no_maximum_value};
        Error -> Error
    end;
max_val(Tree) when is_record(Tree, leaf) ->
    #leaf{value = Val} = Tree,
    Val;
max_val(_Tree) ->
    {error, not_a_binary_tree}.

is_binary_tree_ordered(Tree) when is_record(Tree, binary_tree) ->
    case verify_if_a_binary_tree_is_ordered(Tree) of
        {true, _, _} -> {ok, true};
        {false, _, _} -> {ok, false}
    end;
is_binary_tree_ordered(Tree) when is_record(Tree, leaf) ->
    {ok, true};
is_binary_tree_ordered(_Tree) ->
    {error, not_a_binary_tree}.

insert_value_to_ordered_tree(Tree, InsertVal) when is_record(Tree, binary_tree) ->
    case is_binary_tree_ordered(Tree) of
        {ok, false} -> {error, not_a_ordered_binary_tree};
        {ok, true} -> 
            {ok, insert_value(Tree, InsertVal)};
        Error -> Error
    end;
insert_value_to_ordered_tree(Tree, InsertVal) when is_record(Tree, leaf) ->
    #leaf{value = Val} = Tree,
    if
        InsertVal > Val -> {ok, #binary_tree{value = Val, right_child = #leaf{value = InsertVal}}};
        InsertVal =< Val -> {ok, #binary_tree{value = Val, left_child = #leaf{value = InsertVal}}}
    end;
insert_value_to_ordered_tree(_Tree, _InsertVal) ->
    {error, not_a_binary_tree}.    
%% ====================================================================
%% Internal functions
%% ====================================================================
get_sum(#binary_tree{value = Val, left_child = LeftChild, right_child = RightChild}, _Sum) ->
    Val + get_sum(LeftChild, _Sum) + get_sum(RightChild, _Sum);
get_sum(#leaf{value = Val}, _Sum) ->
    Val;
get_sum(0, _Sum) ->
    0.

find_max_val(#binary_tree{value = Val, left_child = LeftChild, right_child = RightChild}) ->
    maximum(maximum(find_max_val(LeftChild), find_max_val(RightChild)), {not_duplicated, Val});
find_max_val(#leaf{value = Val}) ->
    {not_duplicated, Val};
find_max_val(0) ->
    {not_duplicated, 0}.

maximum(A, B) ->
    {_, AVal} = A,
    {_, BVal} = B,
    if
        AVal > BVal -> A;
        BVal > AVal -> B;
        AVal == BVal -> {duplicated, AVal}
    end.

verify_if_a_binary_tree_is_ordered(#binary_tree{value = Val, left_child = LeftChild, right_child = RightChild}) ->
    if 
        LeftChild == 0 ->
            {IsLeftOrdered, MinLeftVal, MaxLeftVal} = {true, Val, Val};
        true ->
            {IsLeftOrdered, MinLeftVal, MaxLeftVal} = verify_if_a_binary_tree_is_ordered(LeftChild)
    end,
    if 
        RightChild == 0 ->
            {IsRightOrdered, MinRightVal, MaxRightVal} = {true, Val, Val};
        true ->
            {IsRightOrdered, MinRightVal, MaxRightVal} = verify_if_a_binary_tree_is_ordered(RightChild)
    end,
    if
        IsLeftOrdered == false -> {false, 0, ?INFINITY};
        IsRightOrdered == false -> {false, 0, ?INFINITY};
        MaxLeftVal > Val -> {false, 0, ?INFINITY};
        MinRightVal < Val -> {false, 0, ?INFINITY};
        true ->
            {true, MinLeftVal, MaxRightVal}
    end;
verify_if_a_binary_tree_is_ordered(#leaf{value = Val}) ->
    {true, Val, Val};
verify_if_a_binary_tree_is_ordered(_Tree) ->
    {false, 0, ?INFINITY}.

insert_value(#binary_tree{value = Val, left_child = LeftChild, right_child = RightChild}, InsertVal) ->
    if
        InsertVal > Val ->
            case RightChild of
                0 ->
                    #binary_tree{value = Val, left_child = LeftChild, right_child = #leaf{value = InsertVal}};
                _ ->
                    #binary_tree{value = Val, left_child = LeftChild, right_child = insert_value(RightChild, InsertVal)}
            end;
        InsertVal =< Val ->
            case LeftChild of
                0 ->
                    #binary_tree{value = Val, left_child = #leaf{value = InsertVal}, right_child = RightChild};
                _ ->
                    #binary_tree{value = Val, left_child = insert_value(LeftChild, InsertVal), right_child = RightChild}
            end
    end;
insert_value(#leaf{value = Val}, InsertVal) ->
    if
        InsertVal > Val ->
            #binary_tree{value = Val, right_child = #leaf{value = InsertVal}};
        InsertVal =< Val ->
            #binary_tree{value = Val, left_child = #leaf{value = InsertVal}}
    end.