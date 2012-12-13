%% @author ivershinina
%% @doc @todo Add description to quickSort.


-module(sorting).

%% ====================================================================
%% API functions
%% ====================================================================
-export([quickSort/1, mergeSort/1]).

quickSort([]) ->
	[];
quickSort([H|T]) ->
	SmallerList = construct_smaller_list(T, H),
	BiggestList = construct_biggest_list(T, H),
	lists:concat([quickSort(SmallerList), [H], quickSort(BiggestList)]).

mergeSort([]) ->
	[];
mergeSort(List) when is_list(List), length(List) =:= 1 ->
	List;
mergeSort(List) ->
	Length = length(List),
	StartList = mergeSort(lists:sublist(List, Length div 2)),
	EndList = mergeSort(lists:sublist(List, Length div 2 + 1, Length)),
	% compare elements in StartList and EndList
	compare_elements(StartList, EndList).	

%% ====================================================================
%% Internal functions
%% ====================================================================
construct_smaller_list([], _Val) -> 
	[];
construct_smaller_list([H|T], Val) ->
	if 
		H > Val  ->
			construct_smaller_list(T, Val);
		true     ->
			[H|construct_smaller_list(T, Val)]
	end.

construct_biggest_list([], _Val) -> 
	[];
construct_biggest_list([H|T], Val) ->
	if
		H > Val ->
			[H|construct_biggest_list(T, Val)];
		true ->
			construct_biggest_list(T, Val)
	end.
	
compare_elements(List, []) -> List;
compare_elements([], List) -> List;
compare_elements([H1|T1], [H2|T2]) ->
	if
		H1 < H2 -> [H1| compare_elements(T1, [H2|T2])];
		true    -> [H2| compare_elements([H1|T1], T2)]
	end.
	

