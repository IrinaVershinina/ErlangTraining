%% @author ivershinina
%% @doc @todo Add description to expressions.


-module(expressions).

%% ====================================================================
%% API functions
%% ====================================================================
-export([evaluator/1]).

evaluator([H|_T]) ->
	%[H] - the first symbol
	case [H] of 
		"+" -> {plus};
		"-" -> {minus};
		"*" -> {multi};
		"~" -> {unarminus};
		"(" -> {Sign, Val1, Val2}
	    Val -> {num, list_to_integer(Val)}
	end.
	

%% ====================================================================
%% Internal functions
%% ====================================================================

