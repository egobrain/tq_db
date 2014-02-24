-module(tq_dsl).

-export([
         'query'/4,
         model_query/4
        ]).

model_query(PoolName, Model, Query, QueryArgs) ->
    Driver = tq_db:get_pool_driver(PoolName),
    case Driver:parse(Model, Query) of
        {ok, {Sql, Fields, DbTypes}} ->
            Args = lists:zipwith(fun(W, F) -> W(F) end, DbTypes, QueryArgs),
            Driver:'query'(PoolName, Sql, Args, Model:constructor(Fields));
        {error, _Reason} = Err -> Err
    end.

'query'(PoolName, Query, QueryArgs, Constructor) ->
    Driver = tq_db:get_pool_driver(PoolName),
    case Driver:parse(undefined, Query) of
        {ok, {Sql, _Fields, DbTypes}} ->
            Args = lists:zipwith(fun(W, F) -> W(F) end, DbTypes, QueryArgs),
            tq_sql:'query'(PoolName, Sql, Args, Constructor);
        {error, _Reason} = Err -> Err
    end.
