%% @author ivershinina
%% @doc @todo Add description to echo_server_test.


-module(echo_server_test).

-include_lib("eunit/include/eunit.hrl").
-import(echo_server, [start/0, stop/0, print/1]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

print_test() ->
    start(),
    ?assertEqual(ok, print(a)),
    stop().

print_not_started_test() ->
    ?assertError(badarg, print(a)).

%% ====================================================================
%% Internal functions
%% ====================================================================