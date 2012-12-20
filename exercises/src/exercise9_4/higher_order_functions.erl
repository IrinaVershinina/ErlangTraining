%% @author ivershinina
%% @doc @todo Add description to higher_order_functions.


-module(higher_order_functions).

%% ====================================================================
%% API functions
%% ====================================================================
-export([all/2, any/2, dropwhile/2, filter/2, foldl/3, foldr/3, map/2, partition/2]).

all(_Predicate, []) ->
    true;
all(Predicate, [H|T]) ->
    case Predicate(H) of
        true -> all(Predicate, T);
        false -> false
    end.

any(_Predicate, []) ->
    false;
any(Predicate, [H|T]) ->
    case Predicate(H) of
        false -> any(Predicate, T);
        true -> true
    end.

dropwhile(_Predicate, []) ->
    [];
dropwhile(Predicate, [H|T]) ->
    case Predicate(H) of
        true -> dropwhile(Predicate, T);
        false -> [H|T]
    end.

filter(_Predicate, []) ->
    [];
filter(Predicate, [H|T]) ->
    case Predicate(H) of
        true ->
            [H|filter(Predicate, T)];
        false ->
            filter(Predicate, T)
    end.

foldl(F, Acc, [H|T]) ->
    foldl(F, F(H, Acc), T);
foldl(F, Acc, []) when is_function(F, 2) ->
    Acc.

foldr(F, Acc, [H|T]) ->
    F(H, foldr(F, Acc, T));
foldr(F, Acc, []) when is_function(F, 2) ->
    Acc.

map(_Fun, []) ->
    [];
map(Fun, [H|T]) ->
    [Fun(H)|map(Fun, T)].

partition(Predicate, List) ->
    partition(Predicate, List, [], []).
%% ====================================================================
%% Internal functions
%% ====================================================================
partition(_Predicate, [], TrueList, FalseList) ->
    [reverse_list(TrueList), reverse_list(FalseList)];
partition(Predicate, [H|T], TrueList, FalseList) ->
    case Predicate(H) of
        true ->
            partition(Predicate, T, [H|TrueList], FalseList);
        false ->
            partition(Predicate, T, TrueList, [H|FalseList])
    end.

reverse_list([]) -> [];
reverse_list(L) ->
    reverse_list(L, []).

reverse_list([], Acc) -> Acc;
reverse_list([H|T], Acc) ->
    reverse_list(T, [H|Acc]).