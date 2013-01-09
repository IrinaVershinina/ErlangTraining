%% @author ivershinina
%% @doc @todo Add description to gc_test.
-module(gc_test).

%% API
-export([average_1/1, average_2/1]).

average_1(List)  ->
    sum(List) / len(List).

sum([]) ->
    0;
sum([Head | Tail]) ->
    Head + sum(Tail).

len([]) ->
    0;
len([_Head | Tail]) ->
    1 + len(Tail).

average_2(List) ->
    average_acc(List, 0,0).
average_acc([], Sum, Length) ->
    Sum / Length;
average_acc([H | T], Sum, Length) ->
    average_acc(T, Sum + H, Length + 1).
