%% @author ivershinina
%% @doc @todo Add description to higher_order_functions.


-module(higher_order_functions).

%% ====================================================================
%% API functions
%% ====================================================================
-export([print_integers/1, print_smaller_integers/2, print_even_integers/1, concatenate/1, sum/1]).

print_integers(N) ->
    lists:foreach(fun(K) -> io:format("Number: ~p~n", [K]) end, lists:seq(1, N)).

print_smaller_integers(ListOfIntegers, Int) when is_list(ListOfIntegers) ->
    lists:filter(fun(K) -> 
                          if
                              K =< Int -> true;
                              K > Int -> false
                          end 
                  end, ListOfIntegers).

print_even_integers(N) ->
    lists:foreach(fun(K) when K rem 2 == 0 ->
                          io:format("Even number: ~p~n", [K]);
                     (_K) -> ok
                  end, lists:seq(1, N)).

concatenate(ListOfLists) ->
    lists:foldr(fun(K, Acc) -> 
                        lists:foldr(fun(E, Accum) -> [E|Accum] end, Acc, K)
                end, [], ListOfLists).

sum(ListOfIntegers) ->
    lists:foldl(fun(K, Acc) -> K + Acc end, 0, ListOfIntegers).
%% ====================================================================
%% Internal functions
%% ====================================================================