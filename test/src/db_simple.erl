-module(db_simple).
-compile({parse_transform, tq_sqlmodel_transform}).

-field({index, [index,
				required,
				{type, integer},
				{db_type, number},
				{db_alias, <<"id">>}]}).

-field({name, [required,
			   {type, binary},
			   {db_type, number},
			   {db_alias, <<"id">>}]}).

-model([{table, <<"simple_table">>},
		{generate, [get, delete]}
	   ]).

f() ->
	A = 3,
	tq_sql:q(model1, "select @f1, @f2($model2.f1), @* from $model3 where $model4.f2 = #model5.f3{A} and a = #{A}").



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

