
-module(tq_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(tq_db, pools),
    ParseOptsFun =
        fun({Name, Driver, SizeArg, WorkerArg}) ->
                Worker = tq_sql_worker,
                PoolArgs = [{name, {local, Name}},
                            {worker_module, Worker}] ++ SizeArg,
                poolboy:child_spec(Name, PoolArgs, {Driver, WorkerArg})
        end,
    PoolSpecs = lists:map(ParseOptsFun, Pools),
    {ok, { {one_for_one, 5, 10}, PoolSpecs} }.
