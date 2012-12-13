%% @author ivershinina
%% @doc @todo Add description to db.

-module(db).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, destroy/1, write/3, read/2, delete/2, match/2]).

new() ->
	[].

destroy(_Db) ->
	ok.
%Key - is unique field!
write(Key, Element, Db) ->
	case read(Key, Db) of 
		{ok, _}    -> Db;
		{error, _} -> [{Key, Element}|Db]
	end.
	

read(_Key, []) ->
	{error, instance};
read(Key, [H|T]) ->
	case H of
		{Key, _element} ->
			{ok, _element};
		_               ->
			read(Key, T)
	end.

delete(Key, Db) ->
	delete(Key, Db, []).

match(_Element, []) ->
	[];
match(Element, [H|T]) ->
	List = case H of
			   {Key, Element} ->
				   [Key|match(Element, T)];
			   _  			   ->
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
		_               ->
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
