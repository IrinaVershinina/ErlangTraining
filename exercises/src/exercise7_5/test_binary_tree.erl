%% @author ivershinina
%% @doc @todo Add description to test_binary_tree.


-module(test_binary_tree).
-include("records.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0, test1/0, test_ordered_tree/0, test_complex_ordered_tree/0, run_all_tests/0]).

run_all_tests() ->
	test(),
	io:format("~n~n", []),
	test1(),
	io:format("~n~n", []),
	test_ordered_tree(),
	io:format("~n~n", []),
	test_complex_ordered_tree().

test() ->
    Tree = #binary_tree{value = 3,
                        left_child = #binary_tree{value = 4,
                                                  left_child = #leaf{value = 1},
                                                  right_child = #leaf{value = 2}},
                        right_child = #leaf{value =  3}},
    io:format("Tree: ~p~n", [Tree]),
    Sum = binary_tree:sum(Tree),
    io:format("Sum: ~p~n", [Sum]),
    MaxVal = binary_tree:max_val(Tree),
    io:format("MaxVal: ~p~n", [MaxVal]),
    IsBinaryTreeOrdered = binary_tree:is_binary_tree_ordered(Tree),
    io:format("Is binary tree ordered: ~p~n", [IsBinaryTreeOrdered]),
    NewTree = binary_tree:insert_value_to_ordered_tree(Tree, 5),
    io:format("Insert value 5. New Tree: ~p~n", [NewTree]).

test1() ->
    Tree = #binary_tree{value = 4,
                        left_child = #leaf{value = 1},
                        right_child = #leaf{value = 2}},
    io:format("Tree: ~p~n", [Tree]),
    Sum = binary_tree:sum(Tree),
    io:format("Sum: ~p~n", [Sum]),
    MaxVal = binary_tree:max_val(Tree),
    io:format("MaxVal: ~p~n", [MaxVal]),
    IsBinaryTreeOrdered = binary_tree:is_binary_tree_ordered(Tree),
    io:format("Is binary tree ordered: ~p~n", [IsBinaryTreeOrdered]),
    NewTree = binary_tree:insert_value_to_ordered_tree(Tree, 3),
    io:format("Insert value 3. New Tree: ~p~n", [NewTree]).

test_ordered_tree() ->
    Tree = #binary_tree{value = 3,
                        left_child = #leaf{value =  2},
                        right_child = #binary_tree{value = 4,
                                                  left_child = #leaf{value = 3},
                                                  right_child = #leaf{value = 5}}},
    io:format("Tree: ~p~n", [Tree]),
    Sum = binary_tree:sum(Tree),
    io:format("Sum: ~p~n", [Sum]),
    MaxVal = binary_tree:max_val(Tree),
    io:format("MaxVal: ~p~n", [MaxVal]),
    IsBinaryTreeOrdered = binary_tree:is_binary_tree_ordered(Tree),
    io:format("Is binary tree ordered: ~p~n", [IsBinaryTreeOrdered]),
    NewTree = binary_tree:insert_value_to_ordered_tree(Tree, 1),
    io:format("Insert value 1. New Tree: ~p~n", [NewTree]).

test_complex_ordered_tree() ->
    Tree = #binary_tree{value = 10,
                        left_child = #binary_tree{value = 4,
                                                  left_child = #binary_tree{value = 1, 
                                                                            right_child = #leaf{value = 2}},
                                                  right_child = #binary_tree{value = 8,
                                                                             left_child = #leaf{value = 7},
                                                                             right_child = #leaf{value = 9}}},
                        right_child = #binary_tree{value = 17,
                                                   left_child = #binary_tree{value = 14,
                                                                             right_child = #leaf{value = 15}},
                                                   right_child = #binary_tree{value = 21,
                                                                              left_child = #leaf{value = 19}}}},
    io:format("Tree: ~p~n", [Tree]),
    Sum = binary_tree:sum(Tree),
    io:format("Sum: ~p~n", [Sum]),
    MaxVal = binary_tree:max_val(Tree),
    io:format("MaxVal: ~p~n", [MaxVal]),
    IsBinaryTreeOrdered = binary_tree:is_binary_tree_ordered(Tree),
    io:format("IsBinaryTreeOrdered: ~p~n", [IsBinaryTreeOrdered]),
    NewTree = binary_tree:insert_value_to_ordered_tree(Tree, 12),
    io:format("Insert value 12. New Tree: ~p~n", [NewTree]).

%% ====================================================================
%% Internal functions
%% ====================================================================


