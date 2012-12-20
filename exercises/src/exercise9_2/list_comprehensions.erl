%% @author ivershinina
%% @doc @todo Add description to list_comprehensions.


-module(list_comprehensions).

%% ====================================================================
%% API functions
%% ====================================================================
-export([divisible_by_three/1, remove_not_integers/1, lists_intersection/2, symetric_difference/2]).

divisible_by_three(List) ->
    [X || X <- List, X rem 3 == 0].

remove_not_integers(List) ->
    [X || X <- List, is_integer(X)].

lists_intersection(List1, List2) ->
    [X || X <- List1, Y <- List2, X == Y].

symetric_difference(List1, List2) ->
    [R || R<-List1, not lists:member(R, List2)] ++ [R||R<-List2, not lists:member(R, List1)].
%% ====================================================================
%% Internal functions
%% ====================================================================


