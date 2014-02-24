-module(tq_db).

-export([
         start/0,
         stop/0,
         get_pool_driver/1
        ]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

get_pool_driver(PoolName) ->
    {ok, Pools} = application:get_env(tq_db, pools),
    case lists:keyfind(PoolName, 1, Pools) of
        false ->
            throw({unknown_pool, PoolName});
        Tuple when is_tuple(Tuple) ->
            element(2, Tuple)
    end.
