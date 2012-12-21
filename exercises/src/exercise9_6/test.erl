%% @author ivershinina
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test1/0, test2/0, test3/0]).

test1() ->
    Tree = {node,
               {node,
                   {leaf, cat},
                   {node,
                       {leaf, dog},
                       {leaf, emu}
                   }
               },
               {leaf, fish}
           },
    execute(Tree).

test2() ->
        Tree = {node,
                   {leaf, cat},
                   {node,
                       {leaf, dog},
                       {leaf, emu}
                   }
               },
    execute(Tree).

test3() ->
    Tree = {node,
            {leaf, dog},
            {leaf, cat}
           },
    execute(Tree).

execute(Tree) ->
    List = binary_tree_serialization:tree_to_list(Tree),
    Bin = binary_tree_serialization:serialize(List),
    List1 = binary_tree_serialization:deserialize(Bin),
    Tree1 = binary_tree_serialization:list_to_tree(List1),
    io:format("Tree: ~p~n", [Tree]),
    io:format("List: ~p~n", [List]),
    io:format("Serialization: Binary: ~p~n", [Bin]),
    io:format("Deserialization: List: ~p~n", [List1]),
    io:format("Deserialization: Tree: ~p~n", [Tree1]).
%% ====================================================================
%% Internal functions
%% ====================================================================