%% @author ivershinina
%% @doc @todo Add description to call_handler.


-module(call_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, terminate/1, handle_event/2]).

init(File) ->
    {ok, Fd} = file:open(File, [write,append]),
    Fd.

terminate(Fd) ->
    file:close(Fd).

handle_event({State, Id, Originated_person}, Fd) ->
    print(Fd, State, Id, Originated_person),
    Fd;
handle_event(_, Fd) ->
    Fd.

%% ====================================================================
%% Internal functions
%% ====================================================================
print(Fd, State, Id, Originated_person) ->
    Date = fmt(date()), Time = fmt(time()),
    io:format(Fd, "~s, ~s, ~w, ~w, ~w~n", 
                [Date, Time, State, Id, Originated_person]).

fmt({AInt, BInt, CInt}) ->
    AStr = pad(integer_to_list(AInt)),
    BStr = pad(integer_to_list(BInt)),
    CStr = pad(integer_to_list(CInt)),
    [AStr, $:, BStr, $:, CStr].

pad([M1]) -> [$0, M1];
pad(Other) -> Other.