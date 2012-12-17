%% @author ivershinina
%% @doc @todo Add description to event_manager.


-module(event_manager).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, get_data/2, send_event/2, swap_handlers/4]).
-export([init/1]).

start(Name, HandlerList) ->
    register(Name, spawn(event_manager, init, [HandlerList])), ok.

init(HandlerList) ->
    loop(initialize(HandlerList)).

stop(Name) ->
    Name ! {stop, self()},
    receive
        {reply, Reply} -> Reply
    end.

add_handler(Name, Handler, InitData) ->
    call(Name, {add_handler, Handler, InitData}).

delete_handler(Name, Handler) ->
    call(Name, {delete_handler, Handler}).

get_data(Name, Handler) ->
    call(Name, {get_data, Handler}).

send_event(Name, Event) ->
    call(Name, {send_event, Event}).

swap_handlers(Name, OldHandler, NewHandler, InitData) ->
    call(Name, {swap_handlers, OldHandler, NewHandler, InitData}).
%% ====================================================================
%% Internal functions
%% ====================================================================
initialize([]) -> [];
initialize([{Handler, InitData}|Rest]) ->
    [{Handler, Handler:init(InitData)}|initialize(Rest)].

call(Name, Msg) ->
    Name ! {request, self(), Msg},
    receive
        {reply, Reply} -> Reply
    end.

reply(To, Msg) ->
    To ! {reply, Msg}.

loop(State) ->
    receive
        {request, From, Msg} ->
            {Reply, NewState} = handle_msg(Msg, State),
            reply(From, Reply),
            loop(NewState);
        {stop, From} ->
            reply(From, terminate(State))
    end.

terminate([]) -> [];
terminate([{Handler, Data}|Rest]) ->
    [{Handler, Handler:terminate(Data)}|terminate(Rest)].

handle_msg({add_handler, Handler, InitData}, LoopData) ->
    {ok, [{Handler, Handler:init(InitData)}|LoopData]};
handle_msg({delete_handler, Handler}, LoopData) ->
    case lists:keysearch(Handler, 1, LoopData) of
        false ->
            {{error, instance}, LoopData};
        {value, {Handler, Data}} ->
            Reply = {data, Handler:terminate(Data)},
            NewLoopData = lists:keydelete(Handler, 1, LoopData),
            {Reply, NewLoopData}
    end;
handle_msg({get_data, Handler}, LoopData) ->
    case lists:keysearch(Handler, 1, LoopData) of
        false ->
            {{error, instance}, LoopData};
        {value, {Handler, Data}} ->
            {{data, Data}, LoopData}
    end;
handle_msg({send_event, Event}, LoopData) ->
    {ok, event(Event, LoopData)};
handle_msg({swap_handlers, OldHandler, NewHandler, InitData}, LoopData) ->
    case lists:keysearch(OldHandler, 1, LoopData) of
        false ->
            {{error, invalid_old_handler}, LoopData};
        {value, {OldHandler, Data}} ->
             case OldHandler:terminate(Data) of 
                ok ->
                    NewLoopData = lists:keydelete(OldHandler, 1, LoopData),
                    {ok, [{NewHandler, NewHandler:init(InitData)}|NewLoopData]};
                {error, _Reason} ->
                    {{error, instance}, LoopData}
              end
    end.

event(_Event, []) -> [];
event(Event, [{Handler, Data}|Rest]) ->
    [{Handler, Handler:handle_event(Event, Data)}|event(Event, Rest)].