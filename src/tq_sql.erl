-module(tq_sql).

-export([scan/1]).
-compile(export_all).

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
        gen_server:call(Worker, {query, Sql, Args, Constructor})
    end).


scan(Data) ->
	scan(Data, [], []).


scan([$@ | Rest], Str, Acc) ->
	scan_cf(Rest,
			fun(Token, Rest2) ->
					Acc2 = case Str of
							   [] -> Acc;
							   _ ->[ {str, lists:reverse(Str)}| Acc]
						   end,
					scan(Rest2, [], [Token | Acc2])
			end);
scan([$$ | Rest], Str, Acc) ->
	scan_alias(Rest,
			fun(Token, Rest2) ->
					Acc2 = case Str of
							   [] -> Acc;
							   _ ->[ {str, lists:reverse(Str)}| Acc]
						   end,
					scan(Rest2, [], [Token | Acc2])
			end);
scan([$# | Rest], Str, Acc) ->
	scan_type(Rest,
			  fun(Token, Rest2) ->
					  Acc2 = case Str of
								 [] -> Acc;
								 _ ->[ {str, lists:reverse(Str)}| Acc]
							 end,
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
				  braced_string(Rest3,
								fun(Str, Rest4) ->
										Fun({cf, Field, Str}, Rest4)
								end);
			 (Field, Rest3) ->
				  Fun({cf, Field}, Rest3)
		  end).

%% Scan field alias / table alias

scan_alias(Rest, Fun) ->
	token(Rest,
		  fun(Model, [$. | Rest2]) ->
				  token(Rest2,
						fun(Field, Rest3) ->
								Fun({field_alias, Model, Field}, Rest3)
						end);
			 (Model, Rest2) ->
				  Fun({table_alias, Model}, Rest2)
		  end).

%% Scan type

scan_type(Rest, Fun) ->
	token(Rest,
		  fun(Field, [${ | Rest2]) ->
				  acc_forms_str(Rest2,
								fun(FormsStr, Rest3) ->
										Fun({field_type, Field, FormsStr}, Rest3)
								end);
			 (_Field, _Rest) ->
				  {error, "Type forms must be specified in {"}
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


braced_string(Data, Fun) ->
	braced_string(Data, Fun, [], 0).

braced_string([$(=C|Rest], Fun, Acc, BrCnt) ->
	braced_string(Rest, Fun, [C|Acc], BrCnt+1);
braced_string([$)|Rest], Fun, Acc, 0) ->
	Fun(lists:reverse(Acc), Rest);
braced_string([$)=C|Rest], Fun, Acc, BrCnt) ->
	braced_string(Rest, Fun, [C|Acc], BrCnt-1);	
braced_string([C|Rest], Fun, Acc, BrCnt) ->
	braced_string(Rest, Fun, [C|Acc], BrCnt);
braced_string([], _Fun, _Acc, _BrCnt) ->
	{error, "enclosed br ("}.

acc_forms_str(Data, Fun) ->
	acc_forms_str(Data, Fun, []).

acc_forms_str([$}|Rest], Fun, Acc) ->
	Fun(lists:reverse(lists:flatten(Acc)), Rest);
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
	
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan_test_() ->
	Tests =
		[{"", []},
		 {"test", [{str, "test"}]},
		 {"@field", [{cf, "field"}]},
		 {"@field(bcde)", [{cf, "field", "bcde"}]},

		 {"$test", [{table_alias, "test"}]},
		 {"$test.a", [{field_alias, "test", "a"}]},

		 {"#field{test}", [{field_type, "field", "test"}]},
		 {"#field{\"\"}", [{field_type, "field", "\"\""}]},
		 {"#field{\"test\"}", [{field_type, "field", "\"test\""}]},
		 {"#field{\"test 1,2,4 -s }{}{\"}", [{field_type, "field", "\"test 1,2,4 -s }{}{\""}]},
		 {"#field{'test 1,2,4 -s }{}{'}", [{field_type, "field", "'test 1,2,4 -s }{}{'"}]}
		],
	[{D, fun() -> ?assertEqual({ok, R}, scan(D)) end} || {D, R} <- Tests].
			 

%% b, "select @a from $b where $a = #a(A)."

-endif.
