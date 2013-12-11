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

-include("include/ast_helpers.hrl").

-include_lib("tq_transform/include/record_model.hrl").
-include("include/db_model.hrl").

-export([build_model/1, meta_clauses/1]).

-define(atom_join(A, B), list_to_atom(atom_to_list(A) ++ "_" ++ atom_to_list(B))).
-define(prefix_set(A), ?atom_join(set, A)).
-define(changed_suffix(A), ?atom_join(A, '$changed')).


build_model(Model) ->
    Builders = [
                fun build_get/1,
                fun build_save/1,
                fun build_find/1,
                fun build_delete/1,

                fun build_internal_functions/1
               ],
    lists:foldl(fun(F, {IBlock, FBlock}) ->
                        {IB, FB} = F(Model),
                        {[IB | IBlock], [FB | FBlock]}
                end, {[], []}, Builders).

build_get(#db_model{
             get=true,
             module=Module,
             fields=Fields,
             funs=#funs{get=GetName}
            }) ->
    IndexFields = [F || F <- Fields, F#db_field.is_index =:= true],
    Vars = ["Var" ++ integer_to_list(I) || I <- lists:seq(1, length(IndexFields))],

    GetFun = ?function(GetName,
                       [?clause([?var(V) || V <- Vars], none,
                                [
                                 begin
                                     ModuleStr = atom_to_list(Module),
                                     IndexFields2 = lists:zip(Vars, IndexFields),
                                     Where = [begin
                                                  FieldStr = atom_to_list(F#db_field.name),
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

build_save(#db_model{
              save=true,
              before_save=BeforeSaveHooks,
              after_create=AfterCreateHooks,
              after_update=AfterUpdateHooks,
              funs=#funs{save=SaveName}
             } = Model) ->
    BeforeAst = apply_success_hooks(BeforeSaveHooks ++ ['$save_hook'], ?var('Model')),

    BodyAst =
        case AfterCreateHooks =:= [] andalso AfterUpdateHooks =:= [] of
            true ->
                [BeforeAst];
            false ->
                [?match(?var('IsNew'), ?apply_(?var('Model'), is_new, [])),
                 ?cases(BeforeAst,
                        [?clause([?ok(?var('ResModel'))], none,
                                 [?cases(?var('IsNew'),
                                         [?clause([?atom(true)], none,
                                                  [apply_success_hooks(AfterCreateHooks, ?var('ResModel'))]),
                                          ?clause([?atom(false)], none,
                                                  [apply_success_hooks(AfterUpdateHooks, ?var('ResModel'))])])]),
                         ?clause([?error(?var('Reason'))], none,
                                 [?error(?var('Reason'))])
                        ])]
        end,
    SaveFun = ?function(SaveName, [?clause([?var('Model')], none, BodyAst)]),
    SaveHook= ?function('$save_hook',
                         [?clause([?var('Model')], [],
                                  [?apply(tq_sqlmodel_runtime, save,
                                          [?apply('$db_changed_fields', [?var('Model')]),
                                           ?var('Model')])])]),
    ExternalFuns =
        [
         SaveFun
        ],
    InternalFuns =
        [
         SaveHook,
         db_changed_fields_function(Model)
        ],
    Exports = ?export_funs(ExternalFuns),
    {Exports, ExternalFuns ++ InternalFuns};
build_save(_Model) ->
    {[], []}.

build_find(#db_model{
              find=true,
              module=Module,
              funs=#funs{find=FindName}
             }) ->
    Sql = "SELECT @* FROM $" ++ atom_to_list(Module) ++ " ",
    FindFun = ?function(FindName,
                        [?clause([?var('Where'), ?var('Args')], none,
                                 [?apply(tq_runtime_sql, model_query,
                                         [?atom(db), ?atom(Module),
                                          ?binary([?abstract(Sql), {?var('Where'), binary}]),
                                          ?var('Args')
                                         ])
                                 ])]),
    Export = ?export_fun(FindFun),
    {[Export], [FindFun]};
build_find(_Model) ->
    {[], []}.

build_delete(#db_model{
                delete=true,
                module=Module,
                fields=Fields,
                before_delete=BeforeHooks,
                after_delete=AfterHooks,
                funs=#funs{delete=DeleteName}
               }) ->
    IndexFields = [F || F <- Fields, F#db_field.is_index =:= true],
    Vars = ["Var" ++ integer_to_list(I) || I <- lists:seq(1, length(IndexFields))],
    IndexFields2 = lists:zip(Vars, IndexFields),
    DeleteClause =
        [?clause([?var('M')], none,
                 [
                  ?match(?record(Module, [?field(F#db_field.name, ?var(V)) || {V, F} <- IndexFields2]), ?var('M')),
                  begin
                      ModuleStr = atom_to_list(Module),
                      Where = [begin
                                   FieldStr = atom_to_list(F#db_field.name),
                                   ["$", ModuleStr, ".", FieldStr, " = #", ModuleStr, ".", FieldStr, "{", Var, "}"]
                               end || {Var, F} <- IndexFields2],
                      Where2 = string:join(Where, " AND "),
                      String = ["DELETE FROM $", ModuleStr, " WHERE ", Where2, ";"],
                      ?cases(?apply(tq_sql, q, [?atom(Module), ?string(lists:flatten(String))]),
                             [?clause([?ok(?underscore)], none,
                                      [function_call(F, [?var('M')]) || F <- AfterHooks]++[?atom(ok)]),
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
    DeleteFun = ?function(DeleteName, BodyAst),
    Export = ?export_fun(DeleteFun),
    {[Export], [DeleteFun]};
build_delete(_Model) ->
    {[], []}.

build_internal_functions(Model) ->
    Funs = [
            field_to_db_function(Model),
            field_from_db_function(Model),
            field_constructor_function(Model),
            constructor1_function(Model)
           ],
    Exports = ?export_funs(Funs),
    {Exports, Funs}.

field_to_db_function(Model) ->
    ?function(field_to_db,
              [?clause([?atom(F#db_field.name), ?var('Val')], none,
                       [apply_hooks(F#db_field.to_db_funs, ?var('Val'))])
               || F <- Model#db_model.fields]).

field_from_db_function(Model) ->
    ?function(field_from_db,
              [?clause([?atom(F#db_field.name), ?var('Val')], none,
                       [apply_hooks(F#db_field.from_db_funs, ?var('Val'))])
               || F <- Model#db_model.fields]).

constructor1_function(#db_model{from_db_funs=InitFuns, module=Module}) ->
    SetIsNotNew = ?record(?var('Model'), Module, [?field('$is_new$', ?atom(false))]),
    FinalForm = apply_hooks(InitFuns, SetIsNotNew),
    ?function(constructor,
              [?clause([?var('Fields')], none,
                       [?match(?var('Constructors'),
                               ?list_comp(?apply(field_constructor, [?var('F')]),
                                          [?generator(?var('F'), ?var('Fields'))])),
                        ?func([?clause([?var('List')], none,
                                       [?match(?var('Model'),
                                               ?apply(lists, foldl,
                                                      [?func([?clause([?tuple([?var('F'), ?var('A')]), ?var('M')], none,
                                                                      [?apply_(?var('F'), [?var('A'), ?var('M')])])]),
                                                       ?apply(new, []),
                                                       ?apply(lists, zip, [?var('Constructors'), ?var('List')])])),
                                        FinalForm
                                       ]
                                      )])])]).

field_constructor_function(#db_model{fields=Fields, module=Module}) ->
    DefaultClasuse = ?clause([?var('Fun')], [?nif_is_function(?var('Fun'))], [?var('Fun')]),
    SetterAst = fun(F) -> ?apply(?prefix_set(F#db_field.name),
                                 [apply_hooks(F#db_field.from_db_funs, ?var('Val')),
                                  ?var('Model')])
                end,
    ?function(field_constructor,
              [?clause([?atom(F#db_field.name)], none,
                       [?func([?clause([?var('Val'), ?var('Model')], none,
                                       case F#db_field.record#record_field.stores_in_record of
                                           true ->
                                               [?match(?var('F'),SetterAst(F)),
                                                ?record(?var('F'), Module, [?field(?changed_suffix(F#db_field.name), ?atom(false))])];
                                           false ->
                                               [SetterAst(F)]
                                       end)])]) ||
                  F <- Fields,
                  F#db_field.record#record_field.setter =/= false
              ] ++ [DefaultClasuse]).

db_changed_fields_function(#db_model{module=Module, fields=Fields}) ->
    ListAst = ?list([?tuple([?atom(F#db_field.name),
                             apply_hooks(
                                F#db_field.to_db_funs,
                                ?access(?var('Model'), Module, F#db_field.name)),
                             ?access(?var('Model'), Module, ?changed_suffix(F#db_field.name))
                            ])
                     || F <- Fields,
                        F#db_field.record#record_field.stores_in_record,
                        F#db_field.record#record_field.setter,
                        F#db_field.record#record_field.mode#access_mode.sw]),
    ?function('$db_changed_fields',
              [?clause([?var('Model')], none,
                       [?list_comp(?tuple([?var('Name'), ?var('Val')]),
                                   [?generator(?tuple([?var('Name'), ?var('Val'), ?var('Changed')]),
                                               ListAst),
                                    ?var('Changed')]
                                  )])]).


meta_clauses(#db_model{table=Table, fields=Fields}) ->
    TableClause = ?clause([?atom(table)], none,
                          [?abstract(Table)]),
    IndexesClause = ?clause([?atom(indexes)], none,
                            [?list([?atom(F#db_field.name) || F <- Fields, F#db_field.is_index =:= true])]),
    DbAliasClause = [?clause([?tuple([?atom(db_alias), ?var('F')])], none,
                             [?cases(?var('F'), [?clause([?atom(F#db_field.name)], none,
                                                         [?abstract(F#db_field.alias)])
                                                 || F <- Fields])])],
    DbTypesClause = [?clause([?tuple([?atom(db_type), ?var('F')])], none,
                             [?cases(?var('F'), [?clause([?atom(F#db_field.name)], none,
                                                         [?abstract(F#db_field.type)])
                                                 || F <- Fields])])],
    DbRFieldsClaues = ?clause([?abstract({db_fields, r})], none,
                              [?list([?atom(F#db_field.name)
                                      || F <- Fields,
                                         F#db_field.record#record_field.mode#access_mode.sr])]),
    DbWFieldsClaues = ?clause([?abstract({db_fields, w})], none,
                              [?list([?atom(F#db_field.name)
                                      || F <- Fields, F#db_field.record#record_field.mode#access_mode.sw])]),
    [$, | SqlRFields] = lists:flatten([[$,, atom_to_quated_string(F#db_field.name)]
                                       || F <- Fields, F#db_field.record#record_field.mode#access_mode.sr]),
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

apply_hooks([], Var) ->
    Var;
apply_hooks([Fun], Var) ->
    function_call(Fun, [Var]);
apply_hooks([Fun|Rest], Var) ->
    apply_hooks(Rest, function_call(Fun, [Var])).

apply_success_hooks([], Var) ->
    ?ok(Var);
apply_success_hooks([Fun], Var) ->
    function_call(Fun, [Var]);
apply_success_hooks(Funs, Var) ->
    ?apply(tq_sqlmodel_runtime, success_foldl,
           [Var, ?list([lambda_function(F) || F <- Funs])]).
