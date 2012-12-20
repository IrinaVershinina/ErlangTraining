%% @author ivershinina
%% @doc @todo Add description to enum.


-module(enum).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test1/0, test2/0]).

-define(ENUM(Name, Values), values(Name) -> Values).

?ENUM(day, [sunday, monday]);
?ENUM(month, [january, february, march]).

contains(Enum, Value) -> 
    lists:member(Value, values(Enum)).

test1() ->
    io:format("Members of enum day: ~p~n", [values(day)]),
    io:format("Members of enum month: ~p~n", [values(month)]).

test2() ->
    case contains(day, monday) of
        true ->
            io:format("~p is a member of day enum~n", [monday]);
        false ->
            io:format("~p is not a member of day enum~n", [monday])
    end,
    case contains(month, july) of
        true ->
            io:format("~p is a member of month enum~n", [july]);
        false ->
            io:format("~p is not a member of month enum~n", [july])
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================