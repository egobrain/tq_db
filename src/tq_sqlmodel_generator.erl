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

-include_lib("tq_transform/include/access_mode.hrl").
-include_lib("tq_transform/include/record_model.hrl").
-include("include/db_model.hrl").

-export([build_model/1, meta_clauses/1]).

-define(atom_join(A, B), list_to_atom(atom_to_list(A) ++ "_" ++ atom_to_list(B))).
-define(prefix_set(A), ?atom_join(set, A)).
-define(changed_suffix(A), ?atom_join(A, '$changed')).


build_model(Model) ->
    PrepareFuns =
        [
         fun hooks_args_to_abstract/1,
         fun fields_converters_to_abstract/1
        ],
    PreparedModel = prepare_model(PrepareFuns, Model),
    Builders = [
                fun build_get/1,
                fun build_save/1,
                fun build_find/1,
                fun build_delete/1,

                fun build_internal_functions/1
               ],
    lists:foldl(fun(F, {IBlock, FBlock}) ->
                        {IB, FB} = F(PreparedModel),
                        {[IB | IBlock], [FB | FBlock]}
                end, {[], []}, Builders).

%% === Prepare =================================================================

prepare_model(PrepareFuns, Model) ->
    lists:foldl(fun(F, M) -> F(M) end, Model, PrepareFuns).

hooks_args_to_abstract(Model) ->
    Indexes =
        [
         #db_model.from_db_funs,
         #db_model.before_save,
         #db_model.after_save,
         #db_model.before_delete,
         #db_model.after_delete
        ],
    Fun =
        fun(E, M) ->
                Hooks = element(E, M),
                NewHooks = [function_args_to_abstract(H) || H <- Hooks],
                setelement(E, M, NewHooks)
        end,
    lists:foldl(Fun, Model, Indexes).

fields_converters_to_abstract(#db_model{fields=Fields}=Model) ->
    NewFields = [field_converters_args_to_abstract(F) || F <- Fields],
    Model#db_model{fields=NewFields}.

field_converters_args_to_abstract(Field) ->
    Indexes =
        [
         #db_field.to_db_funs,
         #db_field.from_db_funs
        ],
    Fun =
        fun(E, M) ->
                Hooks = element(E, M),
                NewHooks = [function_args_to_abstract(H) || H <- Hooks],
                setelement(E, M, NewHooks)
        end,
    lists:foldl(Fun, Field, Indexes).

%% === Build ===================================================================

build_get(#db_model{
             get=true,
             module=Module,
             fields=Fields,
             funs=#funs{get=GetName}
            }) ->
    IndexFields = [F || F <- Fields, F#db_field.is_index =:= true],
    Vars = [?var("Var" ++ integer_to_list(I)) || I <- lists:seq(1, length(IndexFields))],

    GetFun = ?function(GetName,
                       [?clause(Vars, none,
                                [
                                 begin
                                     ModuleStr = atom_to_list(Module),
                                     Where = [begin
                                                  FieldStr = atom_to_list(F#db_field.name),
                                                  ["${model}", ModuleStr, ".", FieldStr, " = ~", ModuleStr, ".", FieldStr]
                                              end || F <- IndexFields],
                                     Where2 = string:join(Where, " AND "),
                                     String = ["SELECT @{model}* FROM #", ModuleStr, " as model WHERE ", Where2, " LIMIT 1;"],
                                     ?cases(?apply(tq_runtime_sql, model_query,
                                                   [?atom(db),
                                                    ?atom(Module),
                                                    ?abstract(iolist_to_binary(String)),
                                                    ?list(Vars)]),
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
              after_save=AfterSaveHooks,
              funs=#funs{save=SaveName}
             } = Model) ->
    BeforeAst = apply_success_hooks(BeforeSaveHooks ++ ['$save_hook'], ?var('Model')),
    BodyAst =
        case append_functions_args(AfterSaveHooks, [?var('Model')]) of
            [] ->
                [BeforeAst];
            AfterSaveHooks2 ->
                [?cases(BeforeAst,
                        [?clause([?ok(?var('ResModel'))], none,
                                 [apply_success_hooks(AfterSaveHooks2, ?var('ResModel'))]),
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
    Sql = "SELECT @{model}* FROM #" ++ atom_to_list(Module) ++ " as model ",
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
    Vars = [?var("Var" ++ integer_to_list(I)) || I <- lists:seq(1, length(IndexFields))],
    DeleteClause =
        [?clause([?var('M')], none,
                 [
                  ?match(?record(Module, [?field(F#db_field.name, V) || {V, F} <- lists:zip(Vars, IndexFields)]), ?var('M')),
                  begin
                      ModuleStr = atom_to_list(Module),
                      Where =
                          [begin
                               FieldStr = atom_to_list(F#db_field.name),
                               ["$", ModuleStr, ".", FieldStr, " = ~", ModuleStr, ".", FieldStr]
                           end || F <- IndexFields],
                      Where2 = string:join(Where, " AND "),
                      String = ["DELETE FROM #", ModuleStr, " WHERE ", Where2, ";"],
                      ?cases(?apply(tq_runtime_sql, model_query,
                                    [?atom(db),
                                     ?atom(Module),
                                     ?abstract(iolist_to_binary(String)),
                                     ?list(Vars)]),
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
                                                       ?record(Module, []),
                                                       ?apply(lists, zip, [?var('Constructors'), ?var('List')])])),
                                        apply_hooks(InitFuns, ?var('Model'))
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
                                               case F#db_field.record#record_field.setter of
                                                   true ->
                                                       [?match(?var('F'),SetterAst(F)),
                                                        ?record(?var('F'), Module, [?field(?changed_suffix(F#db_field.name), ?atom(false))])];
                                                   false ->
                                                       [
                                                        ?record(?var('Model'), Module,
                                                                [
                                                                 ?field(F#db_field.name, apply_hooks(F#db_field.from_db_funs, ?var('Val'))),
                                                                 ?field(?changed_suffix(F#db_field.name), ?atom(false))
                                                                ])
                                                       ]
                                               end;
                                           false ->
                                               [SetterAst(F)]
                                       end)])]) ||
                  F <- Fields,
                  F#db_field.record#record_field.getter =/= false
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
                               [?abstract(iolist_to_binary(SqlRFields))]),

    lists:flatten([TableClause,
                   IndexesClause,
                   DbTypesClause,
                   DbAliasClause,
                   DbRFieldsClaues,
                   DbWFieldsClaues,
                   RSqlFieldsClause]).

%% Internal helpers.
function_call({Mod, Fun, FunArgs}, Args) ->
    ?apply(Mod, Fun, FunArgs++Args);
function_call({Fun, FunArgs}, Args) when is_list(FunArgs) ->
    ?apply(Fun, FunArgs++Args);
function_call({Mod, Fun}, Args) ->
    ?apply(Mod, Fun, Args);
function_call(Fun, Args) ->
    ?apply(Fun, Args).

lambda_function({Mod, Fun, FunArgs}) ->
    ?func([?clause([?var('M')], none,
                   [?apply(Mod, Fun, FunArgs ++ [?var('M')])])]);
lambda_function({Fun, FunArgs}) when is_list(FunArgs) ->
    ?func([?clause([?var('M')], none,
                   [?apply(Fun, FunArgs ++ [?var('M')])])]);
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

list_to_abstract(List) ->
    [erl_syntax:abstract(A) || A <- List].

function_args_to_abstract({Mod, Fun, FunArgs}) ->
    {Mod, Fun, list_to_abstract(FunArgs)};
function_args_to_abstract({Fun, FunArgs}) when is_list(FunArgs) ->
    {Fun, list_to_abstract(FunArgs)};
function_args_to_abstract({Mod, Fun}) ->
    {Mod, Fun};
function_args_to_abstract(Fun) ->
    Fun.


append_functions_args(Funs, Args) ->
    [append_function_args(F, Args) || F <- Funs].

append_function_args({Mod, Fun, FunArgs}, Args) ->
    {Mod, Fun, FunArgs++Args};
append_function_args({Fun, FunArgs}, Args) when is_list(FunArgs) ->
    {Fun, FunArgs++Args};
append_function_args({Mod, Fun}, Args) ->
    {Mod, Fun, Args};
append_function_args(Fun, Args) ->
    {Fun, Args}.
