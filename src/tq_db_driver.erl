-module(tq_db_driver).

-callback connect(Args :: any()) -> {ok, Connection :: any()} | {error, Reason :: any()}.

-callback query(Connection, Sql, Args) -> {ok, Rows} | {ok, Count} | {ok, Count, Rows} | {error, Reason} when
	  Connection :: any(),
	  Sql :: string(),
	  Args :: [{atom(), any()}],
	  Count :: non_neg_integer(),
	  Rows :: [tuple(any())],
	  Reason :: any(). 
