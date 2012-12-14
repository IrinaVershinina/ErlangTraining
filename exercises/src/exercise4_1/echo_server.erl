%% @author ivershinina
%% @doc @todo Add description to echo_server.


-module(echo_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, print/1, loop/0]).

start() ->
	register(echo, spawn(echo_server, loop, [])),
    ok.	

print(Term) ->
	echo ! {print, Term},
	ok.

loop() ->
	receive 
		{print, Term} -> 
			io:format("~p~n", [Term]),
			loop();
		stop -> 
			unregister(echo),
			ok;
		_Other -> {error, unknown_message}
	end.

stop() ->
	echo ! stop,
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


