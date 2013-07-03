-module(tq_kv).

-callback connect(Args :: any()) -> {ok, Connection :: any()} | {error, Reason :: any()}.

-callback put(Connection, Key, Value) -> ok | {error, Reason} when
	  Connection :: any(),
	  Key :: binary(),
	  Value :: any(),
	  Reason :: any().

-callback get(Connection, Key) -> {ok, Value} | {error, Reason} when
	  Connection :: any(),
	  Key :: binary(),
	  Value :: any(),
	  Reason :: any(). 

-export([put/3, get/2]).

put(PoolName, Key, Value) ->
	poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {put, Key, Value})
    end).
		
get(PoolName, Key) ->
	poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {get, Key})
    end).
