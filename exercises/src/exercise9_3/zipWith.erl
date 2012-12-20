%% @author ivershinina
%% @doc @todo Add description to zipWith.


-module(zipWith).

%% ====================================================================
%% API functions
%% ====================================================================
-export([zip/2, zipWith/3, add/2, test/0]).

test() ->
    zipWith:zipWith(fun zipWith:add/2, [1,2], [3,4,5]).

zip([], _Val) ->
    [];
zip(_Val, []) ->
    [];
zip([H1|T1], [H2|T2]) ->
    [{H1, H2}|zip(T1, T2)].

add(X, Y) -> X + Y.
zipWith(Add, List1, List2) ->    
    [Add(X, Y) || {X, Y} <- zip(List1, List2)].
%% ====================================================================
%% Internal functions
%% ====================================================================