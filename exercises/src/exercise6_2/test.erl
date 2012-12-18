%% @author ivershinina
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test1/0, test2/0, test1_monitor/0, test2_monitor/0]).

test1() ->
    Pid1 = client:start(),
    Pid2 = client:start(),
    io:format("Starting mutex~n", []),
    mutex_semaphore:start(),
    io:format("Mutex started~n", []),
    io:format("Waiting signal from Pid: ~p~n", [Pid1]),
    sleep(1500),
    client:wait(Pid1),
    io:format("Waiting signal has been sent~n", []),
    io:format("Waiting signal from Pid: ~p~n", [Pid2]),
    sleep(1500),
    client:wait(Pid2),
    io:format("Waiting signal has been sent~n", []),
    io:format("Exit signal to Pid: ~p~n", [Pid1]),
    sleep(1500),
    exit(Pid1, close),
    io:format("Exit signal has been sent~n", []),
    mutex_semaphore:stop(),
    case whereis(mutex) of
        undefined -> ok;
        _ -> unregister(mutex)
    end,
    io:format("Test finished~n", []),
    io:format("Mutex should now be at busy state with Pid: ~p~n", [Pid2]).
    
test2() -> 
    Pid1 = client:start(),
    Pid2 = client:start(),
    io:format("Starting mutex~n", []),
    mutex_semaphore:start(),
    io:format("Mutex started~n", []),
    io:format("Waiting signal from Pid: ~p~n", [Pid1]),
    sleep(1500),
    client:wait(Pid1),
    io:format("Waiting signal has been sent~n", []),
    io:format("Waiting signal from Pid: ~p~n", [Pid2]),
    sleep(1500),
    client:wait(Pid2),
    io:format("Waiting signal has been sent~n", []),
    io:format("Exit signal to Pid: ~p~n", [Pid2]),
    sleep(1500),
    exit(Pid2, close),
    io:format("Exit signal has been sent~n", []),
    io:format("Sending signal (removing the lock) form Pid: ~p~n", [Pid1]),
    client:signal(Pid1),
    io:format("Signal sent~n", []),
    mutex_semaphore:stop(),
    io:format("Test finished~n", []),
    io:format("Mutex should now be at free state~n", []).

test1_monitor() ->
    Pid1 = client:start(),
    Pid2 = client:start(),
    io:format("Starting mutex~n", []),
    mutex_semaphore_monitor:start(),
    io:format("Mutex started~n", []),
    io:format("Waiting signal from Pid: ~p~n", [Pid1]),
    sleep(1500),
    client:wait(Pid1),
    io:format("Waiting signal has been sent~n", []),
    io:format("Waiting signal from Pid: ~p~n", [Pid2]),
    sleep(1500),
    client:wait(Pid2),
    io:format("Waiting signal has been sent~n", []),
    io:format("Exit signal to Pid: ~p~n", [Pid1]),
    sleep(1500),
    exit(Pid1, close),
    io:format("Exit signal has been sent~n", []),
    mutex_semaphore_monitor:stop(),
    case whereis(mutex) of
        undefined -> ok;
        _ -> unregister(mutex)
    end,
    io:format("Test finished~n", []),
    io:format("Mutex should now be at busy state with Pid: ~p~n", [Pid2]).
    
test2_monitor() -> 
    Pid1 = client:start(),
    Pid2 = client:start(),
    io:format("Starting mutex~n", []),
    mutex_semaphore_monitor:start(),
    io:format("Mutex started~n", []),
    io:format("Waiting signal from Pid: ~p~n", [Pid1]),
    sleep(1500),
    client:wait(Pid1),
    io:format("Waiting signal has been sent~n", []),
    io:format("Waiting signal from Pid: ~p~n", [Pid2]),
    sleep(1500),
    client:wait(Pid2),
    io:format("Waiting signal has been sent~n", []),
    io:format("Exit signal to Pid: ~p~n", [Pid2]),
    sleep(1500),
    exit(Pid2, close),
    io:format("Exit signal has been sent~n", []),
    io:format("Sending signal (removing the lock) form Pid: ~p~n", [Pid1]),
    client:signal(Pid1),
    io:format("Signal sent~n", []),
    mutex_semaphore_monitor:stop(),
    io:format("Test finished~n", []),
    io:format("Mutex should now be at free state~n", []).
    
sleep(Timeout) ->
    receive
    after 
        Timeout -> ok
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================