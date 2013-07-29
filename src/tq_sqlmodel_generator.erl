%% Copyright (c) 2011-2013, Jakov Kozlov <xazar.studio@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(tq_sqlmodel_generator).

-include("include/db.hrl").
-include("include/ast_helpers.hrl").

-export([build_model/1, meta_clauses/1]).

build_model(Model) ->
	Builders = [
				fun build_get/1,
				fun build_save/1,
				fun build_find/1,
				fun build_delete/1
			   ],
	lists:foldl(fun(F, {IBlock, FBlock}) ->
						{IB, FB} = F(Model),
						{[IB | IBlock], [FB | FBlock]}
				end, {[], []}, Builders).


build_get(#model{get=true, module=Module, fields=Fields}) ->
	IndexFields = [F || F <- Fields, F#field.is_index =:= true],
	Vars = ["Var" ++ integer_to_list(I) || I <- lists:seq(1, length(IndexFields))],

	GetFun = ?function(get, 
					   [?clause([?var(V) || V <- Vars], none, 
								[
								 begin
									 ModuleStr = atom_to_list(Module),
									 IndexFields2 = lists:zip(Vars, IndexFields),
									 Where = [begin
												  FieldStr = atom_to_list(F#field.name),
												  ["$", ModuleStr, ".", FieldStr, " = #", ModuleStr, ".", FieldStr, "{", Var, "}"]
											  end || {Var, F} <- IndexFields2],
									 Where2 = string:join(Where, " AND "),
									 String = ["SELECT @* FROM $", ModuleStr, " WHERE ", Where2, " LIMIT 1;"],
									 ?cases(?apply(tq_sql, q, [?atom(Module), ?string(lists:flatten(String))]),
								 	 		[?clause([?ok(?list([?var("R")]))], none,
								 	 				 [?ok(?var("R"))]),
								 	 		 ?clause([?ok(?list([]))], none,
								 	 		 		 [?error(?atom(undefined))]),
								 	 		 ?clause([?error(?var("Reason"))], none,
								 	 		 		 [?error(?var("Reason"))])])
									 end
								 ])]),
	Export = ?export_fun(GetFun),
	{[Export], [GetFun]};
build_get(_Model) ->
	{[], []}.

build_save(#model{save=true, before_save=BeforeSaveHook, after_save=AfterSaveHook}) ->
	ActionHook = {tq_sqlmodel_runtime, save},
	ActionsList = lists:flatten([BeforeSaveHook, ActionHook, AfterSaveHook]),
	BodyAst =
		case ActionsList of
			[Fun] ->
				function_call(Fun, [?var('Model')]);
			_ ->
				?apply(tq_sqlmodel_runtime, success_foldl,
					   [?var('Model'), ?list([lambda_function(Fun) || Fun <- ActionsList])])
		end,
	SaveFun = ?function(save, [?clause([?var('Model')], none, [BodyAst])]),
	Export = ?export_fun(SaveFun),
	{[Export], [SaveFun]};
build_save(_Model) ->
	{[], []}.

build_find(#model{find=true}) ->
	Fields = ?apply('$meta', [?abstract({sql, {db_fields, r}})]),
	Table = ?apply('$meta', [?atom(table)]),
	Constructor = ?apply('constructor', [?apply('$meta', [?abstract({db_fields, r})])]),
	Sql = ?list([?string(" SELECT "),
				 Fields,
				 ?string(" FROM "),
				 Table,
				 ?var('Where')
				]),
	FindFun = ?function(find,
						[?clause([?var('Where'), ?var('Args')], none,
								 [
								  begin
									  ?apply(tq_sql, 'query', [?atom(db), Sql, ?var('Args'), Constructor])
								  end
								 ])]),
	Export = ?export_fun(FindFun),
	{[Export], [FindFun]};
build_find(_Model) ->
	{[], []}.

build_delete(#model{delete=true, module=Module, fields=Fields, before_delete=BeforeHooks, after_delete=AfterHooks}) ->
	IndexFields = [F || F <- Fields, F#field.is_index =:= true],
	Vars = ["Var" ++ integer_to_list(I) || I <- lists:seq(1, length(IndexFields))],
	IndexFields2 = lists:zip(Vars, IndexFields),

	DeleteClause =
		[?clause([?record(Module, [?field(F#field.name, ?var(V)) || {V, F} <- IndexFields2])], none,
				 [
				  begin
					  ModuleStr = atom_to_list(Module),
					  Where = [begin
								   FieldStr = atom_to_list(F#field.name),
								   ["$", ModuleStr, ".", FieldStr, " = #", ModuleStr, ".", FieldStr, "{", Var, "}"]
							   end || {Var, F} <- IndexFields2],
					  Where2 = string:join(Where, " AND "),
					  String = ["DELETE FROM $", ModuleStr, " WHERE ", Where2, ";"],
					  ?cases(?apply(tq_sql, q, [?atom(Module), ?string(lists:flatten(String))]),
							 [?clause([?ok(?underscore)], none,
									  [function_call(F, [?var('Model')]) || F <- AfterHooks]++[?atom(ok)]),
							  ?clause([?error(?var("Reason"))], none,
									  [?error(?var("Reason"))])])
				  end
				 ])],

	BodyAst = case BeforeHooks of
				  [] ->
					  DeleteClause;
				  _ ->
					  [?clause([?var('Model')], none,
							   [?apply(tq_sqlmodel_runtime, success_foldl,
									   [?var('Model'),
										?list([lambda_function(F) || F <- BeforeHooks]
											  ++ [?func(DeleteClause)])])]
							  )]
	end,
	GetFun = ?function(delete, BodyAst),
	Export = ?export_fun(GetFun),
	{[Export], [GetFun]};
build_delete(_Model) ->
	{[], []}.

meta_clauses(#model{table=Table, fields=Fields}) ->
	TableClause = ?clause([?atom(table)], none,
							[?abstract(Table)]),
	IndexesClause = ?clause([?atom(indexes)], none,
							[?list([?atom(F#field.name) || F <- Fields, F#field.is_index =:= true])]),
	DbAliasClause = [?clause([?tuple([?atom(db_alias), ?var('F')])], none,
							 [?cases(?var('F'), [?clause([?atom(F#field.name)], none,
														 [?abstract(F#field.alias)])
												 || F <- Fields])])],
	DbTypesClause = [?clause([?tuple([?atom(db_type), ?var('F')])], none,
							 [?cases(?var('F'), [?clause([?atom(F#field.name)], none,
														 [?abstract(F#field.type)])
												 || F <- Fields])])],
	DbRFieldsClaues = ?clause([?abstract({db_fields, r})], none,
							 [?list([?atom(F#field.name)
									 || F <- Fields, F#field.mode#access_mode.sr])]),
	DbWFieldsClaues = ?clause([?abstract({db_fields, w})], none,
							  [?list([?atom(F#field.name)
									  || F <- Fields, F#field.mode#access_mode.sw])]),
	[$, | SqlRFields] = lists:flatten([[$,, atom_to_quated_string(F#field.name)]
										|| F <- Fields, F#field.mode#access_mode.sr]),
	RSqlFieldsClause = ?clause([?abstract({sql, {db_fields, r}})], none,
							   [?string(SqlRFields)]),

	lists:flatten([TableClause,
				   IndexesClause,
				   DbTypesClause,
				   DbAliasClause,
				   DbRFieldsClaues,
				   DbWFieldsClaues,
				   RSqlFieldsClause]).

%% Internal helpers.
function_call({Mod, Fun, FunArgs}, Args) ->
	FunArgs2 = [erl_syntax:abstract(A) || A <- FunArgs],
	?apply(Mod, Fun, FunArgs2++Args);
function_call({Fun, FunArgs}, Args) when is_list(FunArgs) ->
	FunArgs2 = [erl_syntax:abstract(A) || A <- FunArgs],
	?apply(Fun, FunArgs2++Args);
function_call({Mod, Fun}, Args) ->
	?apply(Mod, Fun, Args);
function_call(Fun, Args) ->
	?apply(Fun, Args).

lambda_function({Mod, Fun, FunArgs}) ->
	FunArgs2 = [erl_syntax:abstract(A) || A <- FunArgs],
	?func([?clause([?var('M')], none,
				   [?apply(Mod, Fun, FunArgs2 ++ [?var('M')])])]);
lambda_function({Fun, FunArgs}) when is_list(FunArgs) ->
	FunArgs2 = [erl_syntax:abstract(A) || A <- FunArgs],
	?func([?clause([?var('M')], none,
				   [?apply(Fun, FunArgs2 ++ [?var('M')])])]);
lambda_function({Mod, Fun}) ->
	?func(Mod, Fun, 1);
lambda_function(Fun) ->
	?func(Fun, 1).

atom_to_quated_string(Atom) ->
	lists:flatten("\""++atom_to_list(Atom)++"\"").
