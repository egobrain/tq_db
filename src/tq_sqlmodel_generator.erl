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

	ActionList = lists:flatten([BeforeSaveHook, ActionHook, AfterSaveHook]),
	{ActionList2, _} = lists:mapfoldl(fun(Fun, Cnt) ->
										 Var = ?var(list_to_atom("Model"++integer_to_list(Cnt))),
										 {wrap_ast_if_defined(Fun, Var), Cnt+1}
								 end, 1, ActionList),
	Final = fun(VarName) -> [?ok(VarName)] end,

	BodyAstFun = lists:foldr(fun(Fun, Acc) ->
									 Fun(Acc)
							 end, Final, ActionList2),

	BodyAst = BodyAstFun(?var('Model')),
	SaveFun = ?function(save, [?clause([?var('Model')], none, BodyAst)]),
	Export = ?export_fun(SaveFun),
	{[Export], [SaveFun]};
build_save(_Model) ->
	{[], []}.

wrap_ast_if_defined(Function, NextVarName) ->
	Case = fun(CaseFun, Next, VarName) ->
				   ?cases(CaseFun,
						  [?clause([?ok(VarName)], none,
								   Next(VarName)),
						   ?clause([?error(?var('Reason'))], none,
								   [?error(?var('Reason'))])])
		   end,
	fun(Next) ->
			fun(VarName) ->
					case Function of
						undefined ->
							Next(VarName);
						{Mod, Fun} ->
							[Case(?apply(Mod, Fun, [VarName]), Next, NextVarName)];
						Fun ->
							[Case(?apply(Fun, [VarName]), Next, NextVarName)]
					end
			end
	end.

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

build_delete(#model{delete=true, module=Module, fields=Fields}) ->
	IndexFields = [F || F <- Fields, F#field.is_index =:= true],
	Vars = ["Var" ++ integer_to_list(I) || I <- lists:seq(1, length(IndexFields))],
	IndexFields2 = lists:zip(Vars, IndexFields),
	GetFun = ?function(delete, 
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
													 [?atom(ok)]),
								 	 		 ?clause([?error(?var("Reason"))], none,
								 	 		 		 [?error(?var("Reason"))])])
									 end
								 ])]),
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

atom_to_quated_string(Atom) ->
	lists:flatten("\""++atom_to_list(Atom)++"\"").
