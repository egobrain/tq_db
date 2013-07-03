
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
	PoolSpecs = lists:map(fun({Name, Driver, SizeArg, WorkerArg}) ->
		PoolArgs = [{name, {local, Name}},
					{worker_module, tq_db_worker}] ++ SizeArg,
		poolboy:child_spec(Name, PoolArgs, {Driver, WorkerArg})
	end, Pools),
    {ok, { {one_for_one, 5, 10}, PoolSpecs} }.

