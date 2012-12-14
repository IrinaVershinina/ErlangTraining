%% @author ivershinina
%% @doc @todo Add description to expressions.

-module(expressions).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0, test/1, parse/1, pretty_printer/1, evaluator/1, compiler/1, simulator/1, simplifier/1]).

test() ->
    test("2"),
    test("(2+3)"),
    test("~2"),
    test("~(2)"),
    test("((2+3)-4)"),
    test("~(4-(2+3))"),
    test("(~(2)*~(3))"),
    test("~((2*3)+(3*4))"),
	test("~((0*5)*(4+5))"),
    [].
test(S) ->
    io:format(
        "expression: ~p~n" ++
        "parsed:     ~p~n" ++
        "evaluated:  ~p~n" ++
        "pretty:     ~p~n" ++
        "compiler:   ~p~n" ++
        "simulated:  ~p~n" ++
		"simplified: ~p~n" ++	
        "~n", [S, parse(S), evaluator(parse(S)), pretty_printer(parse(S)), 
            compiler(parse(S)), simulator(compiler(parse(S))), simplifier(parse(S))]).

parse(S) ->
    {R, _} = do_parse(S),
    R.
	
evaluator(Tuple) ->
	case Tuple of
        {plus, A, B} -> evaluator(A) + evaluator(B);
        {minus, A, B} -> evaluator(A) - evaluator(B);
        {multiply, A, B} -> evaluator(A) * evaluator(B);
        {divide, A, B} -> evaluator(A) / evaluator(B);
        {tilde, A} -> -evaluator(A);
        {num, N} -> N
	end.

pretty_printer(Tuple) ->
    case Tuple of
        {num, N} -> integer_to_list(N);
        {plus, A, B} -> "(" ++ pretty_printer(A) ++ "+" ++ pretty_printer(B) ++ ")";
        {minus, A, B} -> "(" ++ pretty_printer(A) ++ "-" ++ pretty_printer(B) ++ ")";
        {multiply, A, B} -> "(" ++ pretty_printer(A) ++ "*" ++ pretty_printer(B) ++ ")";
        {divide, A, B} -> "(" ++ pretty_printer(A) ++ "/" ++ pretty_printer(B) ++ ")";
        {tilde, A} -> "~" ++ pretty_printer(A)
    end.

compiler(Tuple) ->
    case Tuple of
        {num, N} -> [{push, N}];
        {Op, A, B} -> compiler(A) ++ compiler(B) ++ [{Op}];
        {tilde, A} -> [{push, 0}] ++ compiler(A) ++ [{minus}]
    end.

simulator(L) -> do_simulator(L, []).   

simplifier(Tuple) ->
	case Tuple of 
		{plus, A, B} ->
			SimpleA = simplifier(A),
			SimpleB = simplifier(B),
			if
				SimpleA == {num, 0} -> SimpleB;
				SimpleB == {num, 0} -> SimpleA;
				true -> {plus, SimpleA, SimpleB}
			end;
		{minus, A, B} ->
			SimpleA = simplifier(A),
			SimpleB = simplifier(B),
			if
				SimpleA == {num, 0} -> {tilde, SimpleB};
				SimpleB == {num, 0} -> SimpleA;
				true -> {minus, SimpleA, SimpleB}
			end;
		{multiply, A, B} ->
			SimpleA = simplifier(A),
			SimpleB = simplifier(B),
			if
				SimpleA == {num, 0} -> {num, 0};
				SimpleB == {num, 0} -> {num, 0};
				SimpleA == {num, 1} -> SimpleB;
				SimpleB == {num, 1} -> SimpleA;
				true -> {multiply, SimpleA, SimpleB}
			end;
		{divide, A, B} ->
			SimpleA = simplifier(A),
			SimpleB = simplifier(B),
			if
				SimpleB == {num, 1} -> SimpleA;
				true -> {divide, SimpleA, SimpleB}
			end;
		{tilde, A} ->
			SimpleA = simplifier(A),
			if
				SimpleA == {num, 0} -> {num, 0};
				true -> {tilde, SimpleA}
			end;
		Val -> Val
	end.
 				
%% ====================================================================
%% Internal functions
%% ====================================================================

do_parse([H|T]) ->
    %io:format("do_parse: ~p~n", [[H|T]]),
    case [H] of
        "~" ->
            {A, R} = do_parse(T),
            {{tilde, A}, R};
        "(" ->
            {A, R1} = do_parse(T),
            [Op | R2] = R1,
            case Op of
                $) -> {A, R2};
                _ ->
                    {B, R3} = do_parse(R2),
                    [$) | R4] = R3,
                    {{op(Op), A, B}, R4}
            end;
        _ ->
            {M, _} = string:to_integer([H]),
            {{num, M}, T}
    end.

op($+) -> plus;
op($-) -> minus;
op($*) -> multiply;
op($/) -> divide.

do_simulator([], [R]) -> R;
do_simulator([H|T], List) ->
    case H of
        {push, N} -> do_simulator(T, [N|List]);
        {plus} ->
            [A|List1] = List,
            [B|List2] = List1,
            do_simulator(T, [B + A | List2]);
        {minus} ->
            [A|List1] = List,
            [B|List2] = List1,
            do_simulator(T, [B - A | List2]);
        {multiply} ->
            [A|List1] = List,
            [B|List2] = List1,
            do_simulator(T, [B * A | List2]);
        {divide} ->
            [A|List1] = List,
            [B|List2] = List1,
            do_simulator(T, [B / A | List2])
    end.
