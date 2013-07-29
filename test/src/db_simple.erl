-module(db_simple).
-compile({parse_transform, tq_sqlmodel_transform}).

%% -export([save/1]).

-field({index, [index,
				required,
				{type, integer},
				{db_type, number},
				{db_alias, <<"id">>}]}).

-field({name, [required,
			   {type, binary},
			   {db_type, string},
			   {db_alias, <<"name">>}]}).

-field({tmp, [required,
			  {type_constructor, any_type_constructor},
			  {type, any},
			  {db_type, any}
			 ]}).

-model([{table, <<"simple_table">>},
		{generate, [get, save, find, delete]},

		{before_save, before_save},
		{after_save, after_save},
		{before_delete, before_delete},
		{after_delete, after_delete}
	   ]).

any_type_constructor(A) -> A.

before_save(Model) ->
	{ok, Model:set_tmp({before, Model:tmp()})}.

after_save(Model) ->   
	{ok, Model:set_tmp({'after', Model:tmp()})}.

before_delete(_Model) ->
	ok.
after_delete(_Model) ->
	ok.


f() ->
	A = 3,
	tq_sql:q(db_simple,
			 " SELECT @index, @name($db_simple.index), @* FROM $db_simple"
			 " WHERE $db_simple.id = #db_simple.id{A} AND $db_simple.name LIKE #{\"test\"}").



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

before_after_test() ->
	meck:new(tq_sqlmodel_runtime, [unstick, passthrough]),
	meck:expect(tq_sqlmodel_runtime, 'save',
				fun(M) ->
						{ok, M:set_tmp({save, M:tmp()})}
				end),
	{ok, Model} = ?MODULE:from_proplist([{tmp, data}]),
	{ok, Model2} = Model:save(),
	meck:unload(tq_sqlmodel_runtime),
	?assertEqual({'after', {save, {before, data}}}, Model2:tmp()).

-endif.

