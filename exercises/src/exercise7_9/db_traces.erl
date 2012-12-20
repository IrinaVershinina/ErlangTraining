%% @author ivershinina
%% @doc @todo Add description to db_traces.


-module(db_traces).
-include("records.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0]).

-ifdef(debug).
    -define(DBG_TRACE(Str, Args), io:format(Str, Args)).
-else.
    -define(DBG_TRACE(Str, Args), ok).
-endif.

start() ->
    register(db, spawn(?MODULE, init, [])).

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
        {reply, Reply} -> 
            ?DBG_TRACE("write pair {~p, ~p} operation is executed. Result: ~p~n", [Key, Element, Reply]),
            Reply
    end.

delete(Key) ->
    db ! {request, self(), {delete, Key}},
    receive 
        {reply, Reply} -> 
            ?DBG_TRACE("delete record with key ~p operation is executed. Result: ~p~n", [Key, Reply]),
            Reply
    end.

read(Key) ->
    db ! {request, self(), {read, Key}},
    receive 
        {reply, Reply} -> 
            ?DBG_TRACE("read record by key ~p operation is executed. Result: ~p~n", [Key, Reply]),
            Reply
    end.

match(Element) ->
    db ! {request, self(), {match, Element}},
    receive 
        {reply, Reply} -> 
            ?DBG_TRACE("match record by location ~p operation is executed. Result: ~p~n", [Element, Reply]),
            Reply
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
loop(Db) ->
    receive
        {request, From, {write, Key, Element}} ->
            ?DBG_TRACE("Message: ~p received~n", [{write, Key, Element}]),
            NewDb = do_write(Db, {Key, Element}),
            reply(From, ok),
            loop(NewDb);
        {request, From, {delete, Key}} ->
            ?DBG_TRACE("Message: ~p received~n", [{delete, Key}]),
            NewDb = delete(Db, Key),
            reply(From, ok),
            loop(NewDb);
        {request, From, {read, Key}} ->
            ?DBG_TRACE("Message: ~p received~n", [{read, Key}]),
            Reply = read(Db, Key),
            reply(From, Reply),
            loop(Db);
        {request, From, {match, Element}} ->
            ?DBG_TRACE("Message: ~p received~n", [{match, Element}]),
            List = match(Db, Element),
            reply(From, List),
            loop(Db);
        {request, From, stop} ->
            ?DBG_TRACE("Message: ~p received~n", [stop]),
            unregister(db),
            reply(From, ok)
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

do_write(Db, {Key, Element}) ->
    case read(Db, Key) of 
        {ok, _}    -> Db;
        {error, _} -> [#data{key = Key, data = Element}|Db]
    end.

read([], _Key) ->
    {error, instance};
read([#data{key = Key, data = Val}|_T], Key) ->
    {ok, Val};
read([_H|T], Key) ->
    read(T, Key).

delete(Db, Key) ->
    delete(Db,Key, []).

delete([], _Key, Acc) ->
    Acc;
delete([#data{key = Key, data = _Val}|T], Key, Acc) ->
    Acc ++ T;
delete([H|T], Key, Acc) ->
    delete(T, Key, [H|Acc]).

match([], _Element) ->
    [];
match([#data{key = Key, data = Element}|T], Element) ->
    L = [Key|match(T, Element)],
    flatten(L);
match([_H|T], Element) ->
    L = [match(T, Element)],
    flatten(L).

flatten(List) when is_list(List) ->
    flatten(List, []).

flatten([], Rest) ->
    Rest;
flatten([H|T], Rest) when is_list(H) ->
    flatten(H, flatten(T, Rest));
flatten([H|T], Rest) ->
    [H|flatten(T, Rest)].