%% @author ivershinina
%% @doc @todo Add description to my_db_gen.


-module(my_db_gen).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

write(Key, Element) ->
    gen_server:call(?MODULE, {write, Key, Element}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

read(Key) ->
    gen_server:call(?MODULE, {read, Key}).

match(Element) ->
    gen_server:call(?MODULE, {match, Element}).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, []}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call({write, Key, Element}, _From, State) ->
    {Reply, NewState} = do_write(State, {Key, Element}),
    {reply, Reply, NewState};
handle_call({delete, Key}, _From, State) ->
    {Reply, NewState} = delete(State, Key),
    {reply, Reply, NewState};
handle_call({read, Key}, _From, State) ->
    Reply = read(State, Key),
    {reply, Reply, State};
handle_call({match, Element}, _From, State) ->
    Reply = match(State, Element),
    {reply, Reply, State}.

%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(stop, State) ->
    {stop, normal, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
do_write(Db, {Key, Element}) ->
    case read(Db, Key) of
        {ok, _} -> {{error, already_exists}, Db};
        {error, instance} -> {ok, [{Key, Element}|Db]}
    end.

read(Db, Key) ->
    case lists:keyfind(Key, 1, Db) of
        false -> {error, instance};
        {_Key, Val} -> {ok, Val}
    end.

delete(Db, Key) ->
    {ok, lists:keydelete(Key,1, Db)}.

match(Db, Element) ->
    Fun = fun({_Key, Val}) -> Val =:= Element end,
    lists:flatmap(fun({Key1, _Val1}) -> [Key1] end, lists:filter(Fun, Db)).