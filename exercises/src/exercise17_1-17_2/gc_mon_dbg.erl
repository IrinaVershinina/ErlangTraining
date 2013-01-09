%% @author ivershinina
%% @doc @todo Add description to gc_mon_dbg

-module(gc_mon_dbg).


%% API
-export([start/0, proc/3]).

list() ->
    lists:seq(1,100000).

start() ->
    measure(gc_test, average_1, list()).
%%     measure(gc_test, average_2, list()).

measure(Mod, Fun, Arg) ->
    HandlerFun = fun
        (A = {trace_ts, Pid, gc_start, Start, T1}, _) ->
            {Start, T1};
        (A = {trace_ts, Pid, gc_end, End, T2}, {Start, T1}) ->
            {_, {_,OHS}} = lists:keysearch(old_heap_size, 1, Start),
            {_, {_,OHE}} = lists:keysearch(old_heap_size, 1, End),
            {_, {_,HS}} = lists:keysearch(heap_size, 1, Start),
            {_, {_,HE}} = lists:keysearch(heap_size, 1, End),
            io:format("gc time: ~p, old heap delta: ~p, heap delta: ~p~n",
                [timer:now_diff(T2, T1), OHE - OHS, HE - HS])
    end,
    dbg:tracer(process, {HandlerFun, {null}}),
    dbg:p(self(), [garbage_collection, timestamp]),
    Mod:Fun(Arg).

proc(Mod, Fun, Arg) ->
    receive
        start ->
            Mod:Fun(Arg)
    end.
