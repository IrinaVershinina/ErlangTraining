%% @author ivershinina
%% @doc Working with database. 
%% The module provides possibilities to store, retrieve 
%% and delete elements in database
%% @copyright 2013 Irina Vershinina

-module(db).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, destroy/1, write/3, read/2, delete/2, match/2]).

%% @doc Create  new database. 
%% Returns [] - an empty database
-spec(new() -> []).
new() ->
    [].

%% @doc Destroy database.
%% Returns 'ok'.
-spec(destroy(Db::[tuple()]) -> ok).
destroy(_Db) ->
    ok.

%% @doc Add new record to database.
%% If element with the specified key is already present in database,
%% nothing will be added and old database will be returned.
%% Otherwise, new database is returned
-spec(write(Key::atom(),Element::atom(), Db::[tuple()] | []) -> [tuple()]).
write(Key, Element, Db) ->
    case read(Key, Db) of
        {ok, _} -> Db;
        {error, _} -> [{Key, Element}|Db]
    end.

%% @doc Find record in database by key.
%% Returns {ok, element} tuple - if record with key specified is found in the database.
%% Otherwise, {error, instanse} tuple is returned.
-spec(read(Key::atom(), Db::[tuple()] | []) -> {ok, atom()} | {error, instance}).
read(_Key, []) ->
    {error, instance};
read(Key, [H|T]) ->
    case H of
        {Key, _element} ->
            {ok, _element};
        _ ->
            read(Key, T)
    end.

%% @doc Delete record with the key specified from database.
%% If there is record with the key specified in the database,
%% it is removed and new database is returned.
%% Otherwise old database is returned.
-spec(delete(Key::atom(), Db::[tuple()] | []) -> [tuple()] | []).
delete(Key, Db) ->
    delete(Key, Db, []).

%% @doc Find record in database by element value.
%% Returnes [] if record with the element specified was not found.
%% Otherwise returns list of keys correspondent to the element specified.
-spec(match(Element::atom(), Db::[tuple()] | []) -> [tuple()] | []).
match(_Element, []) ->
    [];
match(Element, [H|T]) ->
    List = case H of
               {Key, Element} ->
                   [Key|match(Element, T)];
               _ ->
                   [match(Element, T)]
           end,
    tools:flatten(List).

%% ====================================================================
%% Internal functions
%% ====================================================================
delete(_Key, [], Acc) ->
    tools:reverse_list(Acc);
delete(Key, [H|T], Acc) ->
    case H of
        {Key, _element} ->
            concatenate(T, Acc);
        _ ->
            delete(Key, T, [H|Acc])
    end.

concatenate(List1, List2) ->
    concatenate(List1, List2, []).

concatenate([], [], Acc) ->
    tools:reverse_list(Acc);
concatenate([H|T], List2, Acc) ->
    concatenate(T, List2, [H|Acc]);
concatenate([], [H|T], Acc) ->
    concatenate([], T, [H|Acc]).