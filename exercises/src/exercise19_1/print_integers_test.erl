%% @author ivershinina
%% @doc @todo Add description to print_integers_test.


-module(print_integers_test).

-include_lib("eunit/include/eunit.hrl").
-import(print_integers, [print_integers/1, print_even_integers/1]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

print5_test() ->
    ?assertEqual(ok, print_integers(5)).

print_minus3_test() ->
    ?assertThrow({'EXIT', {badarith, _}}, print_integers(-3)).

print_even_5_test() ->
    ?assertEqual(ok, print_even_integers(5)).

print_even_minus6_test() ->
    ?assertThrow({'EXIT', {badarith, _}}, print_even_integers(-6)).

print_even_8_test() ->
    ?assertEqual(ok, print_even_integers(8)).
%% ====================================================================
%% Internal functions
%% ====================================================================

