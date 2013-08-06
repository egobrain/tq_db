-module(tq_runtime_sql).

-export(['query'/4, model_query/4, parse_sql/3]).

model_query(PoolName, Model, Sql, Args) ->
	case parse_sql(Model, Sql, Args) of
		{ok, {Sql2, Args2, Fields}} ->
			tq_sql:'query'(PoolName, Sql2, Args2, Model:constructor(Fields));
		{error, _Reason} = Err -> Err
	end.

'query'(PoolName, Sql, Args, Constructor) ->
	case parse_sql(undefined, Sql, Args) of
		{ok, {Sql2, Args2, _Fields}} ->
			tq_sql:'query'(PoolName, Sql2, Args2, Constructor);
		{error, _Reason} = Err -> Err
	end.

-spec parse_sql(Model, Str, Args) -> {ok, {Sql, Types, Fields}} | {error, Reason} when
	  Str :: binary(),
	  Model :: atom(),
	  Args :: [any()],
	  Sql :: binary(),
	  Types :: [any()],
	  Fields :: [atom()],
	  Reason :: any().
parse_sql(Model, Str, Args) ->
	parse(Str, {Model, Args}, {<<>>, [], []}).

parse(<<$@, Rest/binary>>, {Model, _Args} = Opts, {Sql, Types, Fields}) when Model =/= undefined ->
	scan_cf(Rest,
			fun({Field, Str}, Rest2) ->
					parse(Rest2, Opts, {<<Sql/binary, Str/binary>>, Types, [Field|Fields]});
			   ('*', Rest2) ->
					NewFields = Model:'$meta'({db_fields, r}),
					<<$,, Str/binary>> = << <<$,, (Model:'$meta'({db_alias, F}))/binary>> || F <- NewFields >>,
					parse(Rest2, Opts, {<<Sql/binary, Str/binary>>, Types, lists:reverse(NewFields) ++ Fields});
			   ('...', Rest2) ->
					NewFields = Model:'$meta'({db_fields, r}) -- Fields,
					<<$,, Str/binary>> = << <<$,, (Model:'$meta'({db_alias, F}))/binary>> || F <- NewFields >>,
					parse(Rest2, Opts, {<<Sql/binary, Str/binary>>, Types, lists:reverse(NewFields) ++ Fields});
			   (Field, Rest2) ->
					parse(Rest2, Opts, {<<Sql/binary, (Model:'$meta'({db_alias, Field}))/binary>>, Types, [Field|Fields]})
			end);
parse(<<$$, Rest/binary>>, Opts, {Sql, Types, Fields}) ->
	scan_alias(Rest,
			   fun(Alias, Rest2) ->
					   parse(Rest2, Opts, {<<Sql/binary, Alias/binary>>, Types, Fields})
			   end);
parse(<<$~, Rest/binary>>, {Model, [A | Args]}, {Sql, Types, Fields}) ->
	scan_type(Rest,
			   fun(Type, Rest2) ->
					   parse(Rest2, {Model, Args}, {<<Sql/binary, "~s">>, [{Type, A} | Types], Fields})
			   end);
parse(<<>>, _Opts, {Sql, Types, Fields}) ->
	{ok, {Sql,
		  lists:reverse(Types),
		  lists:reverse(Fields)}};
parse(<<C, Rest/binary>>, Opts, {Sql, Types, Fields}) ->
	parse(Rest, Opts, {<<Sql/binary, C>>, Types, Fields}).

%% Scan model field.

scan_cf(<<"...", Rest/binary>>, Fun) ->
	Fun('...', Rest);
scan_cf(<<$*, Rest/binary>>, Fun) ->
	Fun('*', Rest);
scan_cf(Data, Fun) ->
	token(Data,
		  fun(Field, <<$(, Rest/binary>>) ->
				  braced(Rest,
						 fun(Str, Rest2) ->
								 Fun({binary_to_atom(Field), Str}, Rest2)
						 end);
			 (Field, Rest3) ->
				  Fun(binary_to_atom(Field), Rest3)
		  end).

%% Scan field alias / table alias

scan_alias(Data, Fun) ->
	token(Data,
		  fun(ModelStr, <<$., Rest/binary>>) ->
				  token(Rest,
						fun(FieldStr, Rest2) ->
								Model = binary_to_atom(ModelStr),
								Field = binary_to_atom(FieldStr),
								Fun(Model:'$meta'({db_alias, Field}), Rest2)
						end);
			 (ModelStr, Rest) ->
				  Model2 = binary_to_atom(ModelStr),
				  Fun(Model2:'$meta'(table), Rest)
		  end).

%% Scan type

scan_type(Data, Fun) ->
	token(Data,
		  fun(ModelStr, <<$., Rest/binary>>) ->
				  token(Rest,	
						fun(FieldStr, Rest2) ->
								Model = binary_to_atom(ModelStr),
								Field = binary_to_atom(FieldStr),
								Fun(Model:'$meta'({db_type, Field}), Rest2)
						end);
			 (<<>>, _Rest) ->
				  {error, "model field must be specified"};
			 (TypeStr, Rest) ->
				  Fun(binary_to_atom(TypeStr), Rest)
		  end).

token(Data, Fun) ->
	token(Data, Fun, <<>>).

token(<<C,_/binary>>=Data, Fun, Acc) 
  when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
	   C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
	   C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
	   C =:= ${; C =:= $}; C =:= $\s; C =:= $\t; C=:= $.;
	   C < 32; C =:= 127 ->
	Fun(Acc, Data);
token(<<C, Rest/binary>>, Fun, Acc) ->
	token(Rest, Fun, <<Acc/binary, C>>);
token(<<>>, Fun, Acc) ->
	Fun(Acc, <<>>).

braced(Data, Fun) ->
	braced(Data, Fun, <<>>, 0).

braced(<<$$, Rest/binary>>, Fun, Str, BrCnt) ->
	scan_alias(Rest, fun(Alias, Rest2) ->
							 braced(Rest2, Fun, <<Str/binary, Alias/binary>>, BrCnt)
					 end);
braced(<<C, Rest/binary>>, Fun, Str, BrCnt) when C =:= $( ->
	braced(Rest, Fun, <<Str/binary, C>>, BrCnt+1);
braced(<<C, Rest/binary>>, Fun, Str, 0) when C =:= $) ->
	Fun(Str, Rest);
braced(<<C, Rest/binary>>, Fun, Str, BrCnt) when C =:= $) ->
	braced(Rest, Fun, <<Str/binary, C>>, BrCnt-1);	
braced(<<C, Rest/binary>>, Fun, Str, BrCnt) ->
	braced(Rest, Fun, <<Str/binary, C>>, BrCnt);
braced(<<>>, _Fun, _Str, _BrCnt) ->
	{error, "enclosed br ("}.

binary_to_atom(Bin) -> list_to_atom(binary_to_list(Bin)).
