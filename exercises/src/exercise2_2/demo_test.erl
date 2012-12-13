-module(demo_test).
-export([test1/0, test2/0]).

test1() ->
	demo:double(4).

test2() ->
	demo:times(3,5).