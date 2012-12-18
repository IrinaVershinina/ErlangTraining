%% @author ivershinina
%% @doc @todo Add description to mutex_semaphore_monitor.


-module(mutex_semaphore_monitor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).
-export([wait/0, signal/0, wait/1, signal/1]).
-export([init/0]).

start() ->
    register(mutex, spawn(mutex_semaphore, init, [])).

stop() ->
    mutex ! stop.

wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

wait(Pid) ->
    mutex ! {wait, Pid},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

signal(Pid) ->
    mutex ! {signal, Pid}, ok.

init() ->
    process_flag(trap_exit, true),
    free().

free() ->
    io:format("Free."),
    receive
        {wait, Pid} ->
            Reference = erlang:monitor(process, Pid),
            io:format("Monitor started. Pid: ~p", [Pid]),
            Pid ! ok,
            busy(Pid, Reference);
        stop ->
            terminate()
    end.

busy(Pid, Reference) ->
    io:format("Busy. Pid: ~p~n", [Pid]),
    receive
        {signal, Pid} ->
            io:format("Signal. Pid: ~p~n", [Pid]),
            erlang:demonitor(Reference),
            free();
        {'DOWN', Reference, process, Pid, Reason} ->
            io:format("Process Pid: ~p crashed with reason: ~p~n", [Pid, Reason]),
            free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after
        0 -> ok
    end,
    unregister(mutex).
%% ====================================================================
%% Internal functions
%% ====================================================================