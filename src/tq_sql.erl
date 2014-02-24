-module(tq_sql).

-export([
         'query'/2,
         'query'/3,
         'query'/4
        ]).

%% =============================================================================
%%% API functions
%% =============================================================================

'query'(PoolName, Sql) ->
    'query'(PoolName, Sql, []).
'query'(PoolName, Sql, Args) ->
    Tuple = fun(A) -> list_to_tuple(A) end,
    'query'(PoolName, Sql, Args, Tuple).
'query'(PoolName, Sql, Args, Constructor) ->
    Driver = tq_db:get_pool_driver(PoolName),
    Driver:'query'(PoolName, Sql, Args, Constructor).
