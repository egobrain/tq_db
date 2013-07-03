-module(tq_db).

-export([start/0,
		 stop/0]).

-export([query/2, query/3, query/4]).

start() ->
	application:start(?MODULE).

stop() ->
	application:stop(?MODULE).

query(PoolName, Sql) ->
		query(PoolName, Sql, []).
query(PoolName, Sql, Args) ->
		Tuple = fun(A) -> list_to_tuple(A) end,
		query(PoolName, Sql, Args, Tuple).
query(PoolName, Sql, Args, Constructor) ->
	poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {query, Sql, Args, Constructor})
    end).
		
