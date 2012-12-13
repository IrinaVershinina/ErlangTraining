%% @author ivershinina
%% @doc @todo Add description to tools.


-module(tools).

%% ====================================================================
%% API functions
%% ====================================================================
-export([reverse_list/1, flatten/1]).

reverse_list([]) ->
	[];
reverse_list(L) ->
	reverse_list(L, []).

reverse_list([], Acc) ->
	Acc;
reverse_list([H|T], Acc) ->
	reverse_list(T, [H|Acc]).

flatten(List) when is_list(List) ->
	flatten(List, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

flatten([], Rest) ->
	Rest;
flatten([H|T], Rest) when is_list(H) ->
	flatten(H, flatten(T, Rest));
flatten([H|T], Rest) ->
	[H|flatten(T, Rest)].
