%% @author ivershinina
%% @doc @todo Add description to sum.


-module(sum).

%% ====================================================================
%% API functions
%% ====================================================================
-export([sum/1, sum/2]).

sum(N) when N > 1 -> N + sum(N-1);
sum(1)             -> 1.

sum(N, M) when N < M  -> M + sum(N, M-1);
sum(N, M) when N == M -> N;
sum(_N, _M)             -> throw({'EXIT', {badarith, [{sum, sum, 2}]}}).

%% ====================================================================
%% Internal functions
%% ====================================================================


