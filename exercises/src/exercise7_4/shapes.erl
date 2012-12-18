%% @author ivershinina
%% @doc @todo Add description to shapes.


-module(shapes).
-include("records.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([area/1, perimeter/1]).

-define(PI, 3.1415926).

area(#circle{radius = R}) ->
    ?PI*R*R;
area(#rectangle{length=L, width=W}) ->
    L*W;
area(#triangle{a = A, b = B, c = C}) ->
    P = (A + B + C) / 2,
    math:sqrt((P - A)*(P - B)*(P - C)*P);
area(_Shape) ->
    {error, unknown_shape}.

perimeter(#circle{radius = R}) ->
    2*?PI*R;
perimeter(#rectangle{length=L, width=W}) ->
    2*(L + W);
perimeter(#triangle{a = A, b = B, c = C}) ->
    A + B + C;
perimeter(_Shape) ->
    {error, unknown_shape}.
%% ====================================================================
%% Internal functions
%% ====================================================================