-module(tq_db_app).

-behaviour(application).

-export([
         start/2,
         stop/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pools} = application:get_env(tq_db, pools),
    lists:foreach(
      fun({PoolName, Driver, Args}) ->
              Driver:start_pool(PoolName, Args)
      end, Pools),
    tq_db_sup:start_link().

stop(_State) ->
    ok.
