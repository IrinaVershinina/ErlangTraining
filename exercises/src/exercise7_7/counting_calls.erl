%% @author ivershinina
%% @doc @todo Add description to counting_calls.


-module(counting_calls).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0, call/1, stop_counter/0, get_number_of_calls/0]).
-export([init/0]).

-define(CALL_FUNC(Func), increase_counter(), Func).

test() ->
    register(counter, spawn(counting_calls, init, [])),
    ?CALL_FUNC(call(2)),
    ?CALL_FUNC(call(3)),
    ?CALL_FUNC(call(7)),
    ?CALL_FUNC(call(8)),
    ?CALL_FUNC(call(9)),
    ?CALL_FUNC(call(7)),
    ?CALL_FUNC(call(8)),
    ?CALL_FUNC(call(9)),
    Count = get_number_of_calls(),
    io:format("Number of calls: ~p~n", [Count]),
    stop_counter().

get_number_of_calls() ->
    counter ! {request, self(), get_count},
    receive 
        {reply, {count, Count}} -> Count
    after 
        2000 -> timeout
    end.

stop_counter() ->
    counter ! {request, self(), stop},
    unregister(counter).
    
call(IntVal) ->
    Res = IntVal + 3,
    io:format("~p + 3 = ~p~n", [IntVal, Res]).

increase_counter() ->
    counter ! {request, self(), increase}.

init() ->
    loop(0).

loop(Count) ->
    receive
        {request, _From, increase} ->
            loop(Count +1);
        {request, From, get_count} ->
            From ! {reply, {count, Count}},
            loop(Count);
        {request, From, stop} ->
            From ! {reply, ok}
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================