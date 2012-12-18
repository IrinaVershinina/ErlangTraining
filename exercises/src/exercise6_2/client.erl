%% @author ivershinina
%% @doc @todo Add description to client.


-module(client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, crash/1]).
-export([wait/1, signal/1]).
-export([init/0]).

start() ->
    Pid = spawn(client, init, []),
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
            mutex_semaphore:wait(Pid),
            init();
        {signal, Pid} ->
            mutex_semaphore:signal(Pid),
            init();
        {crash, _Pid} ->
            1/0
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================