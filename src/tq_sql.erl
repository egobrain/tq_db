-module(tq_sql).

-include("ast_helpers.hrl").

-export([scan/1, parse_transform/2]).
-compile(export_all).

-define(DBG(F, D), io:format("~p:~p "++F++"~n", [?FILE, ?LINE | D])).

-callback connect(Args :: any()) -> {ok, Connection :: any()} | {error, Reason :: any()}.

-callback query(Connection, Sql, Args) -> {ok, Rows} | {ok, Count} | {ok, Count, Rows} | {error, Reason} when
	  Connection :: any(),
	  Sql :: string(),
	  Args :: [{atom(), any()}],
	  Count :: non_neg_integer(),
	  Rows :: [tuple(any())],
	  Reason :: any(). 

-export([query/2, query/3, query/4]).

query(PoolName, Sql) ->
		query(PoolName, Sql, []).
query(PoolName, Sql, Args) ->
		Tuple = fun(A) -> list_to_tuple(A) end,
		query(PoolName, Sql, Args, Tuple).
query(PoolName, Sql, Args, Constructor) ->
	poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {query, binary_to_list(iolist_to_binary(Sql)), Args, Constructor})
    end).

parse_transform(Ast, _Options)->
	try
		%% ?DBG("~n~p~n=======~n", [Ast]),
		%% ?DBG("~n~s~n=======~n", [pretty_print(Ast)]),
		{Ast2, Errors} = lists:mapfoldl(fun transform_node/2, [], Ast),
		ErrorsAst = [{error, {L, erl_parse, R}} || {L, R} <- Errors],
		Ast3 = ErrorsAst ++ Ast2,
		%% ?DBG("~n~p~n<<<<~n", [Ast3]),
		%% ?DBG("~n~s~n>>>>~n", [tq_transform:pretty_print(Ast3)]),
		Ast3
	catch T:E ->
			Reason = io_lib:format("~p:~p | ~p ~n", [T, E, erlang:get_stacktrace()]),
			[{error, {1, erl_parse, Reason}} | Ast]
	end.

%% Node transform.
transform_node(Node={call,_,{remote,_,{atom,_,tq_sql},{atom,L1,q}},[ModelAst, StringAst]}, Errors) ->
	case [ModelAst, StringAst] of
		[{atom,_,Model},{string,L2,String}] ->
			case scan(String) of
				{ok, Scan} ->
					case transform(Scan, Model) of
						{ok, NewNode} ->
							{erl_syntax:revert(NewNode), Errors};
						{error, Reason} ->
							{Node, [{L2, Reason}|Errors]}
					end;
				{error, Reason} ->
					{Node, [{L2, Reason}|Errors]}
			end;
		_ ->
			{Node, [{L1, "function requires atom and string args"}|Errors]}
	end;
transform_node(Node={call, _, {remote, _, {atom, _, Model}, {atom, L1, efind}}, [StringAst]}, Errors) ->
	case StringAst of
		{string, L2, String} ->
			case scan(String) of
				{ok, Scan} ->
					case transform(Scan, Model, [], [], []) of
						{ok, Ast, [], Types} ->
							ResAst = ?apply(Model,find, [?list(Ast), ?list(Types)]),
							{erl_syntax:revert(ResAst), Errors};
						{ok, _Ast, _, _Types} ->
							Reason = "getter opertaion @ not allowed in efind queries",
							{error, [{L2, Reason}|Errors]};
						{error, Reason} ->
							{Node, [{L2, Reason}|Errors]}
					end;
				{error, Reason} ->
					{Node, [{L2, Reason}|Errors]}
			end;
		_ ->
			{Node, [{L1, "function requires string arg"}|Errors]}
	end;
transform_node(Tuple, Errors) when is_tuple(Tuple) ->
	List = tuple_to_list(Tuple),
	{List2, Errors2} = lists:mapfoldl(fun transform_node/2, Errors, List),
	{list_to_tuple(List2), Errors2};
transform_node(List, Errors) when is_list(List) ->
	lists:mapfoldl(fun transform_node/2, Errors, List);
transform_node(Node, Errors) ->
	{Node, Errors}.


transform(Scan, Model) ->
	case transform(Scan, Model, [], [], []) of
		{ok, Ast, ConstructorFields, Types} ->
			ConstructorAst = ?apply(Model,constructor,[?apply(lists, flatten, [?list(ConstructorFields)])]),
			{ok, ?apply(tq_sql,'query',[?atom(db), ?list(Ast), ?list(Types), ConstructorAst])}
	end.

transform([], _Model, Acc, Constructors, Types) ->
	{ok, lists:reverse(Acc), lists:reverse(Constructors), lists:reverse(Types)};
transform([{str, Str}|Rest], Model, Acc, Constructors, Types) ->
	Ast = ?string(Str),
	transform(Rest, Model, [Ast|Acc], Constructors, Types);
transform([{cf, '*'}|Rest], Model, Acc, Constructors, Types) ->
	Ast = ?apply(Model, '$meta', [?abstract({sql, {db_fields, r}})]),
	FieldsAst = ?apply(Model, '$meta', [?abstract({db_fields, r})]),
	transform(Rest, Model, [Ast|Acc], [FieldsAst|Constructors], Types);
transform([{cf, Field}|Rest], Model, Acc, Constructors, Types) ->
	Ast = ?apply(Model, '$meta', [?abstract({db_alias, Field})]),
	transform(Rest, Model, [Ast|Acc], [?atom(Field)|Constructors], Types);
transform([{cf, Field, Str}|Rest], Model, Acc, Constructors, Types) ->
	{ok, [Ast], [], []} = transform(Str, Model, [], [], []),
	Ast2 = erl_syntax:revert(Ast),
	transform(Rest, Model, [Ast2|Acc], [?atom(Field)|Constructors], Types);
transform([{table_alias, AModel}|Rest], Model, Acc, Constructors, Types) ->
	Ast = ?apply(AModel, '$meta', [?atom(table)]),
	transform(Rest, Model, [Ast|Acc], Constructors, Types);
transform([{field_alias, AModel, Field}|Rest], Model, Acc, Constructors, Types) ->
	Ast = ?apply(AModel, '$meta', [?abstract({db_alias, Field})]),
	transform(Rest, Model, [Ast|Acc], Constructors, Types);
transform([{field_type, AModel, Field, Forms}|Rest], Model, Acc, Constructors, Types) ->
	Ast = ?string("~s"),
	TypeAst = ?tuple([?apply(AModel, '$meta', [?abstract({db_type, Field})]),Forms]),
	transform(Rest, Model, [Ast|Acc], Constructors, [TypeAst|Types]);
transform([{inline, Forms}|Rest], Model, Acc, Constructors, Types) ->
	transform(Rest, Model, [erl_syntax:revert(Forms)|Acc], Constructors, Types).

	

scan(Data) ->
	scan(Data, [], []).

acc_add_string(Acc, []) -> Acc;
acc_add_string(Acc, Str) -> [{str, lists:reverse(Str)}|Acc].

scan([$@ | Rest], Str, Acc) ->
	scan_cf(Rest,
			fun(Token, Rest2) ->
					Acc2 = acc_add_string(Acc, Str),
					scan(Rest2, [], [Token | Acc2])
			end);
scan([$$ | Rest], Str, Acc) ->
	scan_alias(Rest,
			fun(Token, Rest2) ->
					Acc2 = acc_add_string(Acc, Str),
					scan(Rest2, [], [Token | Acc2])
			end);
scan([$# | Rest], Str, Acc) ->
	scan_type(Rest,
			  fun(Token, Rest2) ->
					  Acc2 = acc_add_string(Acc, Str),
					  scan(Rest2, [], [Token | Acc2])
			  end);
scan([], Str, Acc) ->
	Acc2 = case Str of
			   [] -> Acc;
			   _ -> [{str, lists:reverse(Str)} | Acc]
		   end,
	{ok, lists:reverse(Acc2)};
scan([C|Rest], Str, Acc) ->
	scan(Rest, [C | Str], Acc).

%% Scan model field.

scan_cf(Rest, Fun) ->
	token(Rest,
		  fun(Field, [$( | Rest3]) ->
				  braced(Rest3,
						 fun(Str, Rest4) ->
								 Fun({cf, list_to_atom(Field), Str}, Rest4)
						 end);
			 (Field, Rest3) ->
				  Fun({cf, list_to_atom(Field)}, Rest3)
		  end).

%% Scan field alias / table alias

scan_alias(Rest, Fun) ->
	token(Rest,
		  fun(Model, [$. | Rest2]) ->
				  token(Rest2,
						fun(Field, Rest3) ->
								Fun({field_alias, list_to_atom(Model), list_to_atom(Field)}, Rest3)
						end);
			 (Model, Rest2) ->
				  Fun({table_alias, list_to_atom(Model)}, Rest2)
		  end).

%% Scan type

scan_type(Rest, Fun) ->
	token(Rest,
		  fun(Model, [$.|Rest2]) ->
				  token(Rest2,
						fun(Field, [${ | Rest3]) ->
								acc_forms_str(Rest3,
											  fun(FormsStr, Rest4) ->
													  Fun({field_type, list_to_atom(Model), list_to_atom(Field), FormsStr}, Rest4)
											  end);
						   (_Field, _Rest) ->
								{error, "Type forms must be specified in {"}
						end);
			 ([], [${|Rest2]) ->
				  acc_forms_str(Rest2,
								fun(FormsStr, Rest3) ->
										Fun({inline, FormsStr}, Rest3)
								end);
			 (_Model, _Rest3) ->
				  {error, "model field must be specified"}
		  end).

token(Data, Fun) ->
	token(Data, Fun, []).

token([C|_]=Data, Fun, Acc) 
  when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
	   C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
	   C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
	   C =:= ${; C =:= $}; C =:= $\s; C =:= $\t; C=:= $.;
	   C < 32; C =:= 127 ->
	Fun(lists:reverse(Acc), Data);
token([C|Rest], Fun, Acc) ->
	token(Rest, Fun, [C | Acc]);
token([], Fun, Acc) ->
	Fun(lists:reverse(Acc), []).


braced(Data, Fun) ->
	braced(Data, Fun, [], [], 0).

braced([$$|Rest], Fun, Str, Acc, BrCnt) ->
	scan_alias(Rest, fun(Alias, Rest2) ->
							 Acc2 = acc_add_string(Acc, Str),
							 braced(Rest2, Fun, [], [Alias | Acc2], BrCnt)
					 end);
braced([$(=C|Rest], Fun, Str, Acc, BrCnt) ->
	braced(Rest, Fun, [C|Str], Acc, BrCnt+1);
braced([$)|Rest], Fun, Str, Acc, 0) ->
	Acc2 = acc_add_string(Acc, Str),
	Fun(lists:reverse(Acc2), Rest);
braced([$)=C|Rest], Fun, Str, Acc, BrCnt) ->
	braced(Rest, Fun, [C|Str], Acc, BrCnt-1);	
braced([C|Rest], Fun, Str, Acc, BrCnt) ->
	braced(Rest, Fun, [C|Str], Acc, BrCnt);
braced([], _Fun, _Str, _Acc, _BrCnt) ->
	{error, "enclosed br ("}.

acc_forms_str(Data, Fun) ->
	acc_forms_str(Data, Fun, []).

acc_forms_str([$}|Rest], Fun, Acc) ->
	FormsStr = lists:reverse(lists:flatten(Acc)),
	case parse_string(FormsStr) of
		{ok, Form} ->
			Fun(Form, Rest);
		{error, _} = Err ->
			Err
	end;
acc_forms_str([C|Rest], Fun, Acc) when C =:= $"; C =:= $' ->
	acc_until(Rest, C,
			  fun(Str, Rest2) ->
					  acc_forms_str(Rest2, Fun, [Str, C | Acc])
			  end, Acc);
acc_forms_str([C|Rest], Fun, Acc) ->
	acc_forms_str(Rest, Fun, [C|Acc]);
acc_forms_str([], _Fun, _Acc) ->
	{error, "Unclosed {"}.


acc_until([C|Rest], C, Fun, [AC|_] = Acc) when AC =:= $\\ ->
	acc_until(Rest, C, Fun, [C|Acc]);
acc_until([C|Rest], C, Fun, Acc) ->
	Fun([C|Acc], Rest);
acc_until([], C, _Fun, _Acc) ->
	{error, "Unclosed "++[C]};
acc_until([Ch|Rest], C, Fun, Acc) ->
	acc_until(Rest, C, Fun, [Ch | Acc]).

parse_string([]) ->
	{ok, ?list([])};
parse_string(Str) ->
	Str2 = Str ++ ".",
	{ok, Scan, _} = erl_scan:string(Str2),
	try
		case erl_parse:parse_exprs(Scan) of
			{ok, [Form]} ->
				{ok, Form};
			{ok, []} ->
				{error, "interpolation arg required"};
			{ok, _Forms} ->
				{error, "multiple arg in interpolation not allowed"};
			{error, {_, erl_parse, [_, "'.'"]}} ->
				{error, "interpolation syntax error"};
			{error, {_, erl_parse, Reason}} ->
				{error, lists:flatten(Reason)}
		end
	catch _:_ ->
			{error, "interpolation syntax error"}
end.
	
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan_test_() ->
	P = fun(Str) -> {ok, R} = parse_string(Str), R end,
	Tests =
		[{"", []},
		 {"test", [{str, "test"}]},

		 {"@field", [{cf, field}]},
		 {"@field(bcde)", [{cf, field, [{str, "bcde"}]}]},

		 {"$test", [{table_alias, test}]},
		 {"$test.a", [{field_alias, test, a}]},

		 {"#model.field{test}", [{field_type, model, field, P("test")}]},
		 {"#model.field{\"\"}", [{field_type, model, field, P("\"\"")}]},
		 {"#model.field{\"test\"}", [{field_type, model, field, P("\"test\"")}]},
		 {"#model.field{\"test 1,2,4 -s }{}{\"}", [{field_type, model, field, P("\"test 1,2,4 -s }{}{\"")}]},
		 {"#model.field{'test 1,2,4 -s }{}{'}", [{field_type, model, field, P("'test 1,2,4 -s }{}{'")}]},

		 {"#{A}", [{inline, P("A")}]}
		],
	[{D, fun() -> ?assertEqual({ok, R}, scan(D)) end} || {D, R} <- Tests].
			 

%% b, "select @a from $b where $a = #a(A)."

-endif.
