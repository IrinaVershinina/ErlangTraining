%% @author ivershinina
%% @doc @todo Add description to trase_ets

-module(trace_ets).

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start/0]).

start() ->
    ets:new(countries, [ordered_set, named_table]),
    dbg:tracer(),
    Match = dbg:fun2ms(fun
        ([countries, {'EXIT', _}]) -> true;
        ([countries, {'EXIT', _, _}]) -> true end),
    dbg:tp({ets, insert, 2}, Match),
    dbg:p(all, [c]),
    ets:insert(countries, {'ONE', one}),
    ets:insert(countries, {'TWO', two}),
    ets:insert(countries, {'EXIT', failed}),
    ets:insert(countries, {'THREE', three}),
    ets:insert(countries, {'EXIT', terminated}),
    ets:insert(countries, {'EXIT', self(), closed}).
