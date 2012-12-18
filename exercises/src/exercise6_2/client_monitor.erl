%% @author ivershinina
%% @doc @todo Add description to client.


-module(client_monitor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, crash/1]).
-export([wait/1, signal/1]).
-export([init/0]).

start() ->
    Pid = spawn(?MODULE, init, []),
    Pid.

wait(Pid) ->
    Pid ! {wait, Pid}.

signal(Pid) ->
    Pid ! {signal, Pid}.

crash(Pid) ->
    Pid ! {crash, Pid}.

init() ->
    receive
        {wait, Pid} ->
            mutex_semaphore_monitor:wait(Pid),
            init();
        {signal, Pid} ->
            mutex_semaphore_monitor:signal(Pid),
            init();
        {crash, _Pid} ->
            1/0
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================