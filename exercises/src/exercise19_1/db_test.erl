%% @author ivershinina


-module(db_test).

-include_lib("eunit/include/eunit.hrl").
-import(db, [write/3, read/2, new/0, delete/2, match/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% @doc Create  

new_test() ->
    ?assertEqual([], new()).

write_ab_test() ->
    ?assertEqual([{a,b}], write(a, b, [])).

write_cd_test() ->
    ?assertEqual([{c,d}, {a,b}], write(c, d, write(a, b, []))).

read_a_test() ->
    ?assertEqual({ok,b}, read(a, [{c,d}, {a,b}])).

read_nonexistent_test() ->
    ?assertEqual({error, instance}, read(d, [{c,d}, {a,b}])).

delete_test() ->
    ?assertEqual([{a,b}], delete(c, [{c,d}, {a,b}])).

delete_nonexistent_test() ->
    ?assertEqual([{c,d}, {a,b}], delete(d, [{c,d}, {a,b}])).

match_test() ->
    ?assertEqual([b,c], match(a, [{b,a}, {c,a}, {d,g}])).

match_nonexistent_test() ->
    ?assertEqual([], match(d, [{b,a}, {c,a}, {d,g}])).
%% ====================================================================
%% Internal functions
%% ====================================================================