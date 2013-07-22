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
		case escape_args(Args) of
			{ok, Args2} ->
				Indexes = [[$$, integer_to_list(I)] || I <- lists:seq(1, length(Args))],
				Sql2 = lists:flatten(io_lib:format(Sql, Indexes)),
				case pgsql:equery(Conn, Sql2, Args2) of
					{ok, _Columns, Rows} ->
						{ok, Rows};
					{ok, Count} ->
						{ok, Count};
					{ok, Count, _Columns, Rows} ->
						{ok, Count, Rows};
					{error, Reason} ->
						{error, Reason}
				end;
			{error, _} = Err ->
				Err
		end.

escape_args(Args) ->
	tq_transform_utils:error_writer_map(fun escape_arg/1, Args).

escape_arg({integer, Arg}) ->
	case Arg of
		Str when is_list(Str) ->
			case string:to_integer(Str) of
				{Int, []} ->
					{ok, Int};
				_ ->
					%% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid integer\"}]", [Arg]),
					{error, bad_arg}
			end;
		Bin when is_binary(Bin) ->
			escape_arg({integer, binary_to_list(Arg)});
		Int when is_integer(Int) ->
			{ok, Int};
		_ ->
			%% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid integer\"}]", [Arg]),
			{error, bad_arg}
	end;
escape_arg({number, Arg}) ->
	case Arg of
		Str when is_list(Str) ->
			case string:to_integer(Str) of
				{Int, []} ->
					{ok, Int};
				_ ->
					case string:to_float(Str) of
						{Float, []} ->
							{ok, Float};
						_ ->
							%% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid number\"}]", [Arg]),
							{error, bad_arg}
					end
			end;
		Bin when is_binary(Bin) ->
			escape_arg({number, binary_to_list(Arg)});
		Int when is_integer(Int) ->
			{ok, Int};
		Num when is_float(Num) ->
			{ok, Num};
		_ ->
			%% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid number\"}]", [Arg]),
			{error, bad_arg}
	end;
escape_arg({string, Arg}) ->
	case Arg of
		Str when is_list(Str) ->
			{ok, Str};
		Bin when is_binary(Bin) ->
			{ok, Bin};
		Int when is_integer(Int) ->
			{ok, lists:flatten(io_lib:format("'~p'", [Int]))};
		Num when is_float(Num) ->
			{ok, lists:flatten(io_lib:format("'~p'", [Num]))};
		_ ->
			%% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid number\"}]", [Arg]),
			{error, bad_arg}
	end;
escape_arg({datetime, Arg}) ->
	case Arg of
		{{_Y, _M, _D}, {_Hh, _Mm, _Ss}} ->
			{ok, Arg};
		_ ->
			%% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid date\"}]", [Arg]),
			{error, bad_arg}
	end;
escape_arg({boolean, Arg}) ->
	case Arg of
		true ->
			{ok, true};
		false ->
			{ok, false};
		_ ->
			%% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid boolean\"}]", [Arg]),
			{error, bad_arg}
	end.
