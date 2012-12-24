%% @author ivershinina
%% @doc @todo Add description to my_db_supervisor.


-module(my_db_supervisor).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
    SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
    RestartStrategy :: one_for_all
                     | one_for_one
                     | rest_for_one
                     | simple_one_for_one,
    ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
    StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
    RestartPolicy :: permanent
                   | transient
                   | temporary,
    Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    AChild = {my_db_gen,{my_db_gen,start,[]},
              permanent,30000,worker,[my_db_gen]},
    {ok,{{one_for_all,5,3600}, [AChild]}}.