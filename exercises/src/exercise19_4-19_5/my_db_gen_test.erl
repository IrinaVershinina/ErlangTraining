%% @author ivershinina
%% @doc @todo Add description to echo_server_test.


-module(my_db_gen_test).

-include_lib("eunit/include/eunit.hrl").
-import(my_db_gen, [start/0, stop/0, write/2, delete/1, read/1, match/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

prepare() ->
    start(),
    write(foo, bar).

read_baz_test() ->
    prepare(),
    ?assertEqual({error, instance}, read(baz)).

read_foo_test() ->
    ?assertEqual({ok, bar}, read(foo)).

match_bar_test() ->
    ?assertEqual([foo], match(bar)).

write_ab_test() ->
    ?assertEqual(ok, write(a, b)).

write_cd_test() ->
    ?assertEqual(ok, write(c, d)).

read_a_test() ->
    ?assertEqual({ok,b}, read(a)).

read_nonexistent_test() ->
    ?assertEqual({error, instance}, read(d)).

delete_test() ->
    ?assertEqual(ok, delete(c)).

delete_nonexistent_test() ->
    ?assertEqual(ok, delete(d)).

match_test() ->
    ?assertEqual([a], match(b)).

match_nonexistent_test() ->
    ?assertEqual([], match(d)).
