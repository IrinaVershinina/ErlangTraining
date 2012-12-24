%% @author ivershinina
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0]).

test() ->
    system_logging:start(),
    {ok, MsgTime1} = system_logging:message("Hello"),
    sleep(50),
    {ok, MsgTime2} = system_logging:message("Hello"),
    sleep(50),
    {ok, MsgTime3} = system_logging:message("Hello"),
    sleep(50),
    {ok, MsgTime4} = system_logging:message("Hello"),
    sleep(30),
    system_logging:ack(MsgTime1),
    sleep(200),
    system_logging:ack(MsgTime2),
    sleep(10),    
    system_logging:ack(MsgTime3),
    sleep(10),
    system_logging:ack(MsgTime4),
    AverageTime = system_logging:average_time(1000),
    io:format("Average ack time: ~p~n", [AverageTime]),
    system_logging:stop().

%% ====================================================================
%% Internal functions
%% ====================================================================
sleep(Time) ->
    receive
    after 
        Time -> ok
    end.