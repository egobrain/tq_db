-module(db_simple).
-compile({parse_transform, tq_sqlmodel_transform}).

-field({index, [index,
				required,
				{type, integer},
				{db_type, number},
				{db_alias, <<"id">>}]}).

-field({name, [required,
			   {type, binary},
			   {db_type, string},
			   {db_alias, <<"name">>}]}).

-model([{table, <<"simple_table">>},
		{generate, [get, save, find, delete]}
	   ]).

f() ->
	A = 3,
	tq_sql:q(db_simple,
			 " SELECT @index, @name($db_simple.index), @* FROM $db_simple"
			 " WHERE $db_simple.id = #db_simple.id{A} AND $db_simple.name LIKE #{\"test\"}").



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

