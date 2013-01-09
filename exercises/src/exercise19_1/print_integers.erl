%% @author ivershinina
%% @doc @todo Add description to print_integers.


-module(print_integers).

%% ====================================================================
%% API functions
%% ====================================================================
-export([print_integers/1, print_even_integers/1]).

print_integers(N) when is_integer(N), N =:= 1 ->
    io:format("Number: ~p~n", [N]);
print_integers(N) when is_integer(N), N > 1   ->
    print_integers(N-1),
    io:format("Number :~p~n", [N]);
print_integers(_N) ->
    throw({'EXIT', {badarith, [{print_integers, print_integers, 1}]}}).
    

print_even_integers(N) when N =:= 1 ->
    ok;
print_even_integers(N) when is_integer(N), N =:= 2 ->
    io:format("Number: ~p~n", [N]);
print_even_integers(N) when is_integer(N), N > 2, N rem 2 =:= 0 ->
    print_even_integers(N-2),
    io:format("Number: ~p~n", [N]);
print_even_integers(N) when is_integer(N), N > 2, N rem 2 =:= 1 ->
    print_even_integers(N-1);
print_even_integers(_N) ->
    throw({'EXIT', {badarith, [{print_lintegers, print_even_integers, 1}]}}).

%% ====================================================================
%% Internal functions
%% ====================================================================