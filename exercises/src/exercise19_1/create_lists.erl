%% @author ivershinina
%% @doc @todo Add description to create_lists.


-module(create_lists).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, reverse_create/1]).

create(X) ->
    catch create(X, []).
        
reverse_create(X) ->
    L = (catch create(X, [])),
    if 
       is_list(L) -> reverse_list(L);
       true       -> L
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
create(X, List) when X > 1   ->
    create(X-1, [X|List]);
create(X, List) when X =:= 1 ->
    [X|List];
create(X, _List) when X < 1   ->
    throw({'EXIT', {badarith, [{create_lists, create, 2}, 
                            {create_lists, create, 1}]}}).

reverse_list([]) ->
    [];
reverse_list(L) ->
    reverse_list(L, []).

reverse_list([], AccList) ->
    AccList;
reverse_list([H|T], AccList) ->
    reverse_list(T, [H|AccList]).
