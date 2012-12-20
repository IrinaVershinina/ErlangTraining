%% @author ivershinina
%% @doc @todo Add description to pretty_printing.


-module(building_an_index).

%% ====================================================================
%% API functions
%% ====================================================================
-export([index/1]).

-define(Punctuation,"(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

index(File) ->
    ets:new(indexTable, [ordered_set, named_table]),
    processFile(File),
    prettyIndex(),
    ets:delete(indexTable).

%% ====================================================================
%% Internal functions
%% ====================================================================
processFile(File) ->
    {ok, IoDevice} = file:open(File, [read]),
    processLines(IoDevice, 1).

processLines(IoDevice, N) ->
    case io:get_line(IoDevice, "") of
        eof ->
            ok;
        Line ->
            processLine(Line, N),
            processLines(IoDevice, N + 1)
    end.

processLine(Line, N) ->
    Words = re:split(Line, ?Punctuation),
    processWords(Words, N).

processWords(Words, N) ->
    case Words of
        [] -> ok;
        [Word|Rest] ->
            WordStr = binary_to_list(Word), 
            if
                length(WordStr) > 3 ->
                    Normalise = string:to_lower(WordStr),
                    ets:insert(indexTable, {{Normalise, N}});
                true -> ok
            end,
            processWords(Rest, N)
    end.

prettyIndex() ->
    case ets:first(indexTable) of
        '$end_of_table' ->
            ok;
        First ->
            case First of
                {Word, N} ->
                    IndexEntry = {Word, [N]}
            end,
            prettyIndexNext(First, IndexEntry)
    end.

prettyIndexNext(Entry, {Word, Lines}) ->
    Next = ets:next(indexTable, Entry),
    case Next of
        '$end_of_table' ->
            prettyEntry({Word, lists:reverse(Lines)});
        {NextWord, M} ->
            if
                NextWord == Word ->
                    prettyIndexNext(Next, {Word, [M|Lines]});
                true ->
                    prettyEntry({Word, lists:reverse(Lines)}),
                    prettyIndexNext(Next, {NextWord, [M]})
            end
    end.

prettyEntry({Word, Lines}) ->
    TupleLines = accumulate(Lines),
    io:format("~p~n", [pad(15, Word) ++ prettyList(TupleLines)]).

accumulate([H|T]) ->
    accumulate(T, H, H, []).

accumulate([], MinNum, MaxNum, ListOfTuples) ->
    LinesTuple = case MinNum == MaxNum of
                    true -> {MinNum};
                    false -> {MinNum, MaxNum}
                 end,
    lists:reverse([LinesTuple|ListOfTuples]);
accumulate([H|T], MinNum, MaxNum, ListOfTuples) ->
    if
        H == MaxNum + 1 ->
            accumulate(T, MinNum, H, ListOfTuples);
        H == MaxNum ->
            accumulate(T, MinNum, MaxNum, ListOfTuples);
        true ->
            LinesTuple = case MinNum == MaxNum of
                            true -> {MinNum};
                            false -> {MinNum, MaxNum}
                         end,
            accumulate(T, H, H, [LinesTuple|ListOfTuples])
    end.

prettyList([HeadTuple|Tail]) ->
    case size(HeadTuple) of
        2 when Tail == [] -> 
            integer_to_list(element(1, HeadTuple)) ++ "-" ++ integer_to_list(element(2, HeadTuple)) ++ ".";
        2 ->
            integer_to_list(element(1, HeadTuple)) ++ "-" ++ integer_to_list(element(2, HeadTuple)) ++ "," ++ prettyList(Tail);
        1 when Tail == [] ->
            integer_to_list(element(1, HeadTuple)) ++ ".";
        1 ->
            integer_to_list(element(1, HeadTuple)) ++ "," ++ prettyList(Tail)
    end.

pad(N, Word) ->
    Diff = N - length(Word),
    Word ++ construct_N_whitespaces_string(Diff).

construct_N_whitespaces_string(0) ->
    "";
construct_N_whitespaces_string(Count) ->
    " " ++ construct_N_whitespaces_string(Count - 1).