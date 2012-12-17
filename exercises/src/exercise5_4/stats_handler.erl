%% @author ivershinina
%% @doc @todo Add description to stats_handler.


-module(stats_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, terminate/1, handle_event/2]).

init({{Type, Id}, Count}) ->
    {{Type, Id}, Count}.

terminate({{Type, Id}, Count}) ->
    {alarm, {Type, Id}, Count}.

handle_event({Type, Id, _Description}, {{Type, Id}, Count}) ->
    io:format("{~p, ~p} event received one more time", [Type, Id]),
    {{Type, Id}, Count + 1};
handle_event(_Event, Data) ->
    Data.

%% ====================================================================
%% Internal functions
%% ====================================================================