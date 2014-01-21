-module(tq_sql).

-callback connect(Args :: any()) -> {ok, Connection :: any()} | {error, Reason :: any()}.

-callback query(Connection, Sql, Args) -> {ok, Rows} | {ok, Count} | {ok, Count, Rows} | {error, Reason} when
      Connection :: any(),
      Sql :: string(),
      Args :: [{atom(), any()}],
      Count :: non_neg_integer(),
      Rows :: [tuple(any())],
      Reason :: any().

-export([query/2, query/3, query/4]).

'query'(PoolName, Sql) ->
    'query'(PoolName, Sql, []).
'query'(PoolName, Sql, Args) ->
    Tuple = fun(A) -> list_to_tuple(A) end,
    'query'(PoolName, Sql, Args, Tuple).
'query'(PoolName, Sql, Args, Constructor) ->
    TransuctionFun =
        fun(Worker) ->
                gen_server:call(Worker, {'query', Sql, Args, Constructor})
        end,
    poolboy:transaction(PoolName, TransuctionFun).
