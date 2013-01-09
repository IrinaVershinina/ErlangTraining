%% @author ivershinina
%% @doc @todo Add description to sum_tests.


-module(sum_tests).

-include_lib("eunit/include/eunit.hrl").
-import(sum, [sum/1, sum/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

sum1_test() ->
    ?assertEqual(1, sum(1)).

sum0_test() ->
    ?assertError(function_clause, sum(0)).

sum15_test() ->
    ?assertEqual(120, sum(15)).

sum1_5_test() ->
    ?assertEqual(15, sum(1,5)).

sum_minus1_5_test() ->
    ?assertEqual(14, sum(-1,5)).

sum_6_4_test() ->
    ?assertThrow({'EXIT', {badarith, _}}, sum(6,4)).

%% ====================================================================
%% Internal functions
%% ====================================================================