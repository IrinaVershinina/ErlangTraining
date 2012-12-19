%% @author ivershinina
%% @doc @todo Add description to show_eval.


-module(show_eval).

%% ====================================================================
%% API functions
%% ====================================================================
-export([calculate/0]).

-ifdef(show).
    -define(SHOW_EVAL(Expr), io:format("~p = ~p~n", [??Expr, Expr])).
-else.
    -define(SHOW_EVAL(Expr), io:format("~p~n", [Expr])).
-endif.

calculate() ->
   ?SHOW_EVAL(2+3).
%% ====================================================================
%% Internal functions
%% ====================================================================