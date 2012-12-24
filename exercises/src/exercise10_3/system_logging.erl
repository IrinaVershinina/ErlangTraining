%% @author ivershinina
%% @doc @todo Add description to system_logging.


-module(system_logging).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).
-export([init/0]).
-export([message/1, ack/1, average_time/1]).

-record(messages, {msgTime,
                   message,
                   ackReceived = false}).

-record(ack, {ackTime, 
                  mesTime}).

start() ->
    register(log, spawn(?MODULE, init, [])).

stop() ->
    call(stop).

message(Message) ->
    call({message, Message}).

ack(MesTime) ->
    call({ack, MesTime}).

average_time() ->
    average_time(1);
average_time(Time) ->
    Now = now(),
    case ets:last(ackTime) of
        '$end_of_file' -> {error, there_are_no_acks};
        Ack -> get_average_time(Time, Now, ets:lookup(ackTime, Ack), 0, 0)
    end.
    
get_average_time(Time, Now, [#ack{ackTime = AckTime, mesTime = MsgTime}], Acc, Count) ->
    DiffTime = diff_time(Now, AckTime),
    if 
        DiffTime < Time -> 
            case ets:prev(ackTime, AckTime) of
                '$end_of_table' -> Acc / Count;
                PrevAck ->
                    get_average_time(Time, Now, ets:lookup(ackTime, PrevAck), Acc + diff_time(AckTime,MsgTime), Count + 1)
            end;
        Count /= 0 ->
            Acc / Count;
        true ->
            {error, no_acks_in_this_period}
    end.    

diff_time({MegaSec1, Sec1, MicroSec1}, {MegaSec2, Sec2, MicroSec2}) ->
    Time1 = MegaSec1*1000000 + Sec1 + MicroSec1/1000000,
    Time2 = MegaSec2*1000000 + Sec2 + MicroSec2/1000000,
    Time1 - Time2.
%% ====================================================================
%% Internal functions
%% ====================================================================
create_tables() ->
    ets:new(msgAck, [named_table, {keypos, #messages.msgTime}, ordered_set]),
    ets:new(ackTime, [named_table, {keypos, #ack.ackTime}, ordered_set]).

close_tables() ->
    ets:delete(msgAck),
    ets:delete(ackTime).

call(Request) ->
    Ref = make_ref(),
    log ! {request, {self(), Ref}, Request},
    receive
        {reply, Ref, Reply} -> Reply
    end.

reply({From, Ref}, Reply) ->
    From ! {reply, Ref, Reply}.

init() ->
    create_tables(),
    loop().

loop() ->
    receive
        {request, From, stop} ->
            close_tables(),
            reply(From, ok);
        {request, From, Request} ->
            Reply = request(Request),
            reply(From, Reply),
            loop()
    end.

request({message, Message}) ->
    Now = now(),    
    ets:insert(msgAck, #messages{msgTime = Now, message = Message}),
    {ok, Now};
request({ack, MsgTime}) ->
    Now = now(),
    case ets:lookup(msgAck, MsgTime) of
        [] -> 
            {error, ack_to_nonexistant_message};
        [#messages{msgTime = MsgTime, message = _Message, ackReceived = true}] ->
            {error, duplicated_ack};
        [#messages{msgTime = MsgTime, message = _Message, ackReceived = false}] = [Mes] ->
            ets:insert(msgAck, Mes#messages{ackReceived = true}),
            ets:insert(ackTime, #ack{ackTime = Now, mesTime = MsgTime}),
            {ok, Now}
    end.