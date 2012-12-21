%% @author ivershinina
%% @doc @todo Add description to binary_tree_serialization.


-module(binary_tree_serialization).

%% ====================================================================
%% API functions
%% ====================================================================
-export([tree_to_list/1, list_to_tree/1, serialize/1, deserialize/1]).

tree_to_list({leaf, N}) ->
    [2,N];
tree_to_list({node, T1, T2}) ->
    TTL1 = tree_to_list(T1),
    [Size1| _] = TTL1,
    TTL2 = tree_to_list(T2),
    [Size2|List2] = TTL2,
    [Size1+Size2|TTL1 ++ List2].

list_to_tree([_|Ls]) ->
    do_list_to_tree(Ls).

serialize(List) ->
    tree_to_binary(List).

deserialize(Bin) ->
    <<Length:16, _Rest/binary>> = Bin,
    {List, _, _} = binary_to_tree(Bin, Length),
    List.
%% ====================================================================
%% Internal functions
%% ====================================================================
do_list_to_tree([2,N]) ->
    {leaf, N};
do_list_to_tree([N]) ->
    {leaf, N};
do_list_to_tree([M|Rest]) ->
    {Code1, Code2} = lists:split(M-1, Rest),
    {node, do_list_to_tree(Code1), do_list_to_tree(Code2)}.

tree_to_binary([]) ->
    <<>>;
tree_to_binary([Elem]) when is_atom(Elem) ->
    StrLenght = length(atom_to_list(Elem)) + 4,
    list_to_binary([<<StrLenght:16>>, term_to_binary(Elem)]);
tree_to_binary([2, H|T]) ->
    StrLenght = length(atom_to_list(H)) + 4,
    list_to_binary([<<2:16>>, <<StrLenght:16>>, term_to_binary(H), tree_to_binary(T)]);
tree_to_binary([N, Next|Elem]) ->
    {LeftChild, RightChild} = lists:split(Next, [Next|Elem]),
    list_to_binary([<<N:16>>, tree_to_binary(LeftChild), tree_to_binary(RightChild)]).


binary_to_tree(Bin, Length) ->
    if 
        Length > 1 ->
            <<NodeLength:16, Elem/binary>> = Bin,
            case NodeLength of
                2 when Length > 3 ->
                <<StrLength:16, Rest/binary>> = Elem,
                {Text, Rest1} = split_binary(Rest, StrLength),
                {RightChild, Rest2, RightLength} = binary_to_tree(Rest1, Length-NodeLength),
                {[2, binary_to_term(Text)] ++ RightChild, Rest2, Length - NodeLength - RightLength};
                2 when Length == 3 ->
                    <<StrLength:16, Rest/binary>> = Elem,
                    {Text, Rest1} = split_binary(Rest, StrLength),
                    {Term, Rest2, _DiffLength} = binary_to_tree(Rest1, 1),
                    {[2, binary_to_term(Text)] ++ Term, Rest2, 0};
                2 when Length == 2 ->
                    <<StrLength:16, Rest/binary>> = Elem,
                    {Text, Rest1} = split_binary(Rest, StrLength),
                    {[2, binary_to_term(Text)], Rest1, 0};
                N ->
                    {List1, Rest1, _NewLength1} = binary_to_tree(Elem, N - 1),
                    {List2, Rest2, _NewLength2} = binary_to_tree(Rest1, Length - NodeLength),
                    {[N|List1] ++ List2, Rest2, 0}     
            end;
        Length == 1 ->
            <<StrLength:16, Rest/binary>> = Bin,
            {Text, Rest1} = split_binary(Rest, StrLength),
            {[binary_to_term(Text)], Rest1, 0};
        Length == 0 ->
            {[], Bin, 0}
    end.