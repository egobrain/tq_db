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
	scan_mf(Rest,
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
scan([], Str, Acc) ->
	Acc2 = case Str of
			   [] -> Acc;
			   _ -> [{str, lists:reverse(Str)} | Acc]
		   end,
	{ok, lists:reverse(Acc2)};
scan([C|Rest], Str, Acc) ->
	scan(Rest, [C | Str], Acc).

%% Scan model field.

scan_mf(Rest, Fun) ->
	token(Rest,
		  fun(Model, [$. | Rest2]) ->
				  token(Rest2,
						fun(Field, [$( | Rest3]) ->
								braced_string(Rest3,
											  fun(Str, Rest4) ->
													  Fun({mf, Model, Field, Str}, Rest4)
											  end);
						   (Field, Rest3) ->
								Fun({mf, Model, Field}, Rest3)
						end);
			 (_Model, _Rest2) ->
				  {error, "model constructor require fieldname"}
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
	
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan_test_() ->
	Tests =
		[{"", []},
		 {"test", [{str, "test"}]},
		 {"@test.a", [{mf, "test", "a"}]},
		 {"@test.a(bcde)", [{mf, "test", "a", "bcde"}]},

		 {"$test", [{table_alias, "test"}]},
		 {"$test.a", [{field_alias, "test", "a"}]}
		 
		],
	[{D, fun() -> ?assertEqual({ok, R}, scan(D)) end} || {D, R} <- Tests].
			 
	

-endif.
