%% @author ivershinina
%% @doc @todo Add description to my_db.


-module(my_db).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0]).

start() ->
    register(db, spawn(my_db, init, [])).

init() ->
    loop([]).

stop() ->
    db ! {request, self(), stop},
    receive
        {reply, Reply} -> Reply
    end.

write(Key, Element) ->
    db ! {request, self(), {write, Key, Element}},
    receive
        {reply, Reply} -> Reply
    end.

delete(Key) ->
    db ! {request, self(), {delete, Key}},
    receive 
        {reply, Reply} -> Reply
    end.

read(Key) ->
    db ! {request, self(), {read, Key}},
    receive 
        {reply, Reply} -> Reply
    end.

match(Element) ->
    db ! {request, self(), {match, Element}},
    receive 
        {reply, Reply} -> Reply
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
loop(Db) ->
    receive
        {request, From, {write, Key, Element}} ->
            NewDb = do_write(Db, {Key, Element}),
            reply(From, ok),
            loop(NewDb);
        {request, From, {delete, Key}} ->
            NewDb = delete(Db, Key),
            reply(From, ok),
            loop(NewDb);
        {request, From, {read, Key}} ->
            Reply = read(Db, Key),
            reply(From, Reply),
            loop(Db);
        {request, From, {match, Element}} ->
            List = match(Db, Element),
            reply(From, List),
            loop(Db);
        {request, From, stop} ->
            unregister(db),
            reply(From, ok)
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

do_write(Db, {Key, Element}) ->
    case read(Db, Key) of
        {ok, _} -> Db;
        {error, instance} -> [{Key, Element}|Db]
    end.

read(Db, Key) ->
    case lists:keyfind(Key, 1, Db) of
        false -> {error, instance};
        {_Key, Val}   -> {ok, Val}
    end.

delete(Db, Key) ->
    lists:keydelete(Key,1, Db).

match(Db, Element) ->
    Fun = fun({_Key, Val}) -> Val =:= Element end,
    lists:flatmap(fun({Key1, _Val1}) -> [Key1] end, lists:filter(Fun, Db)).