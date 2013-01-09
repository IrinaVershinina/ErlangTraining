%% @author ivershinina
%% @doc @todo Add description to gc_mon

-module(gc_mon).


%% API
-export([start/0, proc/3]).

list() ->
    lists:seq(1,1000).

start() ->
    measure(gc_test, average_1, list()),
    measure(gc_test, average_2, list()).

measure(Mod, Fun, Arg) ->
    Pid = spawn(?MODULE, proc, [Mod, Fun, Arg]),
    erlang:trace(Pid, true, [garbage_collection, timestamp]),
    Pid ! start,
    io:format("~p:~p: ~p microseconds~n", [Mod, Fun, read(0)]).

read(Time) ->
    receive
        {trace_ts, _, gc_start, _, T1} ->
            receive
                {trace_ts, _, gc_end, _, T2} ->
                    read(Time + timer:now_diff(T2, T1))
            end
    after 1000 ->
        Time
    end.

proc(Mod, Fun, Arg) ->
    receive
        start ->
            Mod:Fun(Arg)
    end.
