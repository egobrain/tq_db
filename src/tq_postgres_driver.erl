-module(tq_postgres_driver).

-behavior(tq_sql).

-export([connect/1, query/3]).

connect(Args) ->
	Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    pgsql:connect(Hostname, Username, Password, [
        {database, Database}
    ]).

query(Conn, Sql, Args) ->
	Args2 = [Val || {_Type, Val} <- Args],
	Sql2 = lists:flatten(io_lib:format("~s", [io_lib:format(Sql, Args2)])),
	case pgsql:equery(Conn, Sql2) of
		{ok, _Columns, Rows} ->
			{ok, Rows};
		{ok, Count} ->
			{ok, Count};
		{ok, Count, _Columns, Rows} ->
			{ok, Count, Rows};
		{error, Reason} ->
			{error, Reason}
	end.
