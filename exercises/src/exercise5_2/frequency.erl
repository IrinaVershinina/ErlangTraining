%% @author ivershinina
%% @doc @todo Add description to client_server.


-module(frequency).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).


start() ->
    register(frequency, spawn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

get_frequencies() -> [10,11,12,13,14,15].

stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% ====================================================================
%% Internal functions
%% ====================================================================
call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            Reply = stop(Frequencies),
            reply(Pid, Reply),
            case Reply of
                ok -> ok;
                {error, _} -> loop(Frequencies)
            end
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    Fun = fun({_Key, Val}) -> Val =:= Pid end,
    FreqsAllocatedByThisPid = lists:flatmap(fun({Key1, _Val1}) -> [Key1] end, lists:filter(Fun, Allocated)),
    Len = length(FreqsAllocatedByThisPid),
    if
        Len < 3 ->
            {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
        true ->
            {{[Freq|Free], Allocated}, {error, limit_exceeded}}
    end.

deallocate({Free, Allocated}, Freq, Pid) ->
    case lists:keyfind(Freq, 1, Allocated) of
        {Freq, Pid} ->
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {{[Freq|Free], NewAllocated}, ok};
        _ ->
            {{Free, Allocated}, {error, frequency_cannot_be_deallocated}}
    end.

stop({_Free, Allocated}) ->
    if
        Allocated == [] -> unregister(frequency), ok;
        true -> {error, there_are_allocated_frequencies}
    end.