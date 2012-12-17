%% @author ivershinina
%% @doc @todo Add description to fsm.


-module(fsm).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).
-export([init/0]).
-export([incoming/1, off_hook/0, off_hook/1, other_on_hook/1, on_hook/0, outcoming/1, other_off_hook/1, on_hook/1]).
-export([test/0, test1/0, test2/0]).

test() ->
    start(),
    test1(),
    test2(),
    test2(),
    stop().

test1() ->
    off_hook(),
    outcoming(123),
    other_off_hook(123),
    sleep(2000),
    on_hook(123).

test2() ->
    incoming(124),
    off_hook(124),
    sleep(3000),
    on_hook(124).

start() ->
    event_manager:start(calls, [{call_handler, "CallsLogs"}]),
    register(fsm, spawn(fsm, init, [])), 
    ok.

stop() ->
    call(stop).

init() ->
    idle().

call(Msg) ->
    fsm ! Msg.

incoming(Number) ->
    call({Number, incoming}).

off_hook() ->
    call(off_hook).

off_hook(Number) ->
    call({Number, off_hook}).

other_on_hook(Number) ->
    call({Number, other_on_hook}).

on_hook() ->
    call(on_hook).

outcoming(Number) ->
    call({Number, outcoming}).

other_off_hook(Number) ->
    call({Number, other_off_hook}).

on_hook(Number) ->
    call({Number, on_hook}).

idle() ->
    io:format("State: idle~n"),
    receive
        {Number, incoming} ->
            start_ringing(), 
            ringing(Number);
        off_hook ->
            start_tone(),
            dial();
        stop ->
            unregister(fsm),
            event_manager:stop(calls);
        _Mes -> 
            idle()
            
    end.

ringing(Number) ->
    io:format("State: ringing~n"),
    receive
        {Number, other_on_hook} ->
            stop_ringing(),
            idle();
        {Number, off_hook} ->
            stop_ringing(),
            event_manager:send_event(calls, {connected, Number, incoming_call}),
            connected(Number);
        _Mes -> 
            ringing(Number)    
            
    end.

dial() ->
    io:format("State: dial~n"),
    receive
        on_hook ->
            stop_tone(),
            idle();
        {Number, outcoming} ->
            stop_tone(),
            start_dialing(),
            dialing(Number);
        _Mes -> 
            dial()
            
    end.

dialing(Number) -> 
    io:format("State: dialing~n"),
    receive
        {Number, other_off_hook} ->
            stop_dialing(),
            event_manager:send_event(calls, {connected, Number, outcoming_call}),
            connected(Number);
        {Number, on_hook} ->
            stop_dialing(),
            idle();
        _Mes ->  
            dialing(Number)
            
    end.

connected(Number) ->
    io:format("State: connected~n"),
    receive
        {Number, on_hook} ->
            call_finished(),
            event_manager:send_event(calls, {disconnected, Number, undefined}),
            idle()
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================

start_ringing() ->
    io:format("Start ringing.~n").

stop_ringing() ->
    io:format("Stop ringing.~n").

start_tone() ->
    io:format("Start tone.~n").

stop_tone() ->
    io:format("Stop tone.~n").

start_dialing() ->
    io:format("Start dialing.~n").

stop_dialing() ->
    io:format("Stop dialing.~n").

call_finished() ->
    io:format("Call finished.~n").

sleep(Time) ->
    receive
    after 
        Time -> true
    end.