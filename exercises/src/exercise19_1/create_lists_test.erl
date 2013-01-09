%% @author ivershinina
%% @doc @todo Add description to create_lists_test.


-module(create_lists_test).

-include_lib("eunit/include/eunit.hrl").
-import(create_lists, [create/1, reverse_create/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

create4_test() ->
    ?assertEqual([1,2,3,4], create(4)).

create1_test() ->
    ?assertEqual([1], create(1)).

create0_test() ->
    ?assertMatch({'EXIT', {badarith, _}}, create(0)).

create_minus2_test() ->
    ?assertMatch({'EXIT', {badarith, _}}, create(-2)).

reverse_create4_test() ->
    ?assertEqual([4,3,2,1], reverse_create(4)).

reverse_create1_test() ->
    ?assertEqual([1], reverse_create(1)).

reverse_create0_test() ->
    ?assertMatch({'EXIT', {badarith, _}}, reverse_create(0)).

reverse_create_minus2_test() ->
    ?assertMatch({'EXIT', {badarith, _}}, reverse_create(-2)).

%% ====================================================================
%% Internal functions
%% ====================================================================