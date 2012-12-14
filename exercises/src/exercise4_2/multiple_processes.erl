%% @author ivershinina
%% @doc @todo Add description to multiple_processes.

-module(multiple_processes).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/3, new_process/3]).

start(M, N, Message) ->
	register(main, self()),
	register(first, spawn(multiple_processes, new_process, [N - 1, M, Message])),
	start_listen(N).

new_process(0, M, Message) ->
	send_messages(first, M, Message),
	listen([]);				   
new_process(N, M, Message) ->
	Pid = spawn(multiple_processes, new_process, [N - 1, M, Message]),
	send_messages(Pid, M, Message),
	listen(Pid).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_listen(0) ->
	first ! quit,
    unregister(first),
    unregister(main),
    "done";
start_listen(N) ->
	receive
		{completed, Pid} ->
			io:format("main: completed from pid: ~p~n", [Pid]),
			start_listen(N - 1)
	end.
	
listen(Pid) ->
	receive
		quit ->
			io:format("pid: ~p, quit~n", [self()]),
			case Pid of
				[] -> [];
				_ -> Pid ! quit
			end;
		Mes -> 
			io:format("pid: ~p, received: ~p~n", [self(), Mes]),
			listen(Pid)
	end.
	
send_messages(_Pid, 0, _Message) ->
	main ! {completed, self()};
send_messages(Pid, M, Message) ->
	Pid ! Message,
	send_messages(Pid, M - 1, Message).
