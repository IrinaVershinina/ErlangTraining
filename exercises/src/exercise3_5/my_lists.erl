%% @author ivershinina
%% @doc @todo Add description to my_lists.


-module(my_lists).

%% ====================================================================
%% API functions
%% ====================================================================
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

filter([], _Int) ->
	[];
filter([H|T], Int) when is_integer(Int) ->
	if 
		H > Int -> filter(T, Int);
		true    -> [H|filter(T, Int)]
	end;
filter(_List, _NotInt) ->
	{error, invalid_value}.

reverse([]) ->
	[];
reverse(L) ->
	reverse(L, []).

reverse([], Acc) ->
	Acc;
reverse([H|T], Acc) ->
	reverse(T, [H|Acc]).

concatenate(List) ->
	concatenate(List, []).

flatten(List) when is_list(List) ->
	flatten(List, []).

%% ====================================================================
%% Internal functions
%% ====================================================================
concatenate([], Acc) ->
	reverse(Acc);
concatenate([H|T], Acc) ->
	concatenate(T, add_elements_to_list(H, Acc)).
	
add_elements_to_list([], Acc) ->
	Acc;
add_elements_to_list([H|T], Acc) ->
	add_elements_to_list(T, [H|Acc]).

flatten([], Rest) ->
	Rest;
flatten([H|T], Rest) when is_list(H) ->
	flatten(H, flatten(T, Rest));
flatten([H|T], Rest) ->
	[H|flatten(T, Rest)].
	
