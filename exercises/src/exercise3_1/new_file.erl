%% @author ivershinina
%% @doc @todo Add description to new_file.


-module(new_file).

%% ====================================================================
%% API functions
%% ====================================================================
-export([unsafe/1, preferred/1, even/1, number/1]).

unsafe(X) -> 
	case X of
		one -> Y = true;
		_   -> Y = two
	end,
	Y.

preferred(X) ->
	Y = case X of 
			one -> 12;
			_   -> 196
		end,
	X + Y.

even(Int) when Int rem 2 == 0 -> true;
even(Int) when Int rem 2 == 1 -> false.

number(Num) when is_integer(Num) -> integer;
number(Num) when is_float(Num)   -> float;
number(_Other)                   -> false.


%% ====================================================================
%% Internal functions
%% ====================================================================


