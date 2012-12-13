%% @author ivershinina
%% @doc @todo Add description to db_lists.


-module(db_lists).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, destroy/1, write/3, read/2, delete/2, match/2]).


new() ->
	[].

destroy(_Db) ->
	ok.

write(Key, Element, Db) ->
	case read(Key, Db) of 
		{ok, _}    -> Db;
		{error, _} -> [{Key, Element}|Db]
	end.

read(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		false -> {error, instance};
		{_Key, Val}   -> {ok, Val}
	end.
	
delete(Key, Db) ->
	lists:keydelete(Key,1, Db).

match(Element, List) ->
	Fun = fun({_Key, Val}) -> Val =:= Element end,
	lists:flatmap(fun({Key1, _Val1}) -> [Key1] end, lists:filter(Fun, List)). 

%% ====================================================================
%% Internal functions
%% ====================================================================
