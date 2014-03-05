-module(tq_sqlmodel_transform).

-behavior(tq_transform_plugin).

-include("include/db_model.hrl").
-include_lib("tq_transform/include/access_mode.hrl").
-include_lib("tq_transform/include/record_model.hrl").

-export([parse_transform/2]).

-export([
         create_model/1,
         model_option/3,
         normalize_model/1,
         set_globals/2,
         build_model/1,

         create_field/1,
         field_option/3,
         normalize_field/1,
         set_field/2,

         meta_clauses/1
        ]).

parse_transform(Ast, Options) ->
    try
        tq_transform:parse_transform(Ast, Options, [tq_record_transform, ?MODULE])
    catch T:E ->
            Reason = io_lib:format("~p:~p | ~p ~n", [T, E, erlang:get_stacktrace()]),
            [{error, {1, erl_parse, Reason}} | Ast]
    end.

%% Model.

create_model(Module) ->
    #db_model{module=Module}.

model_option(from_db, NewInitFuns,  #db_model{from_db_funs=InitFuns}=Model) ->
    Model2 = Model#db_model{
               from_db_funs = InitFuns ++ to_list(NewInitFuns)
              },
    {ok, Model2};
model_option(pool_name, PoolName, Model) ->
    Model2 = Model#db_model{pool_name = PoolName},
    {ok, Model2};
model_option(table, Table, Model) ->
    Model2 = Model#db_model{table = Table},
    {ok, Model2};
model_option(generate, Opts, Model) ->
    Model2 = Model#db_model{
               get = proplists:get_value(get, Opts, false),
               save = proplists:get_value(save, Opts, false),
               find = proplists:get_value(find, Opts, false),
               delete = proplists:get_value(delete, Opts, false)
              },
    {ok, Model2};
model_option(module, Module, Model) ->
    Model2 = Model#db_model{module = Module},
    {ok, Model2};
model_option(before_save, Data, Model) ->
    Model2 = Model#db_model{before_save = to_list(Data)},
    {ok, Model2};
model_option(after_save, Data, Model) ->
    Model2 = Model#db_model{after_save = to_list(Data)},
    {ok, Model2};
model_option(before_delete, Data, Model) ->
    Model2 = Model#db_model{before_delete = to_list(Data)},
    {ok, Model2};
model_option(after_delete, Data, Model) ->
    Model2 = Model#db_model{after_delete = to_list(Data)},
    {ok, Model2};
model_option(rename, Aliases, Model) ->
    Funs = fill_names(Aliases, Model#db_model.funs),
    Model2 = Model#db_model{funs = Funs},
    {ok, Model2};
model_option(_Option, _Val, _Model) ->
    false.

normalize_model(Model) ->
    Rules = [
             fun default_pool_name_rule/1,
             fun stores_in_db_rule/1,
             fun table_quoted_rule/1
            ],
    tq_transform_utils:error_writer_foldl(fun(R, M) -> R(M) end, Model, Rules).

set_globals(Globals, #db_model{fields=Fields}=Model) ->
    {ok, TqRecordTransform} = tq_transform:g(plugin, tq_record_transform, Globals),
    Fields2 = [
               begin
                   {ok, RF} = tq_record_transform:g(field, F#db_field.name, TqRecordTransform),
                   F#db_field{record=RF}
               end || F <- Fields
              ],
    Model2 = Model#db_model{fields=Fields2},
    {ok, Model2}.

build_model(Model) ->
    tq_sqlmodel_generator:build_model(Model).

%% Fields.

create_field(Name) ->
    #db_field{name=Name}.

field_option(index, IsIndex, Field) ->
    Field2 = Field#db_field{is_index=IsIndex},
    {ok, Field2};
field_option(db_type, Type, Field) ->
    Field2 = Field#db_field{type = Type},
    {ok, Field2};
field_option(db_alias, Alias, Field) ->
    Field2 = Field#db_field{alias = Alias},
    {ok, Field2};
field_option(from_db, NewInitFuns, #db_field{from_db_funs=InitFuns}=Field) ->
    Field2 = Field#db_field{
               from_db_funs = InitFuns ++ to_list(NewInitFuns)
              },
    {ok, Field2};
field_option(to_db, NewInitFuns, #db_field{to_db_funs=InitFuns}=Field) ->
    Field2 = Field#db_field{
               to_db_funs = InitFuns ++ to_list(NewInitFuns)
              },
    {ok, Field2};
field_option(prevent_select, PreventSelect, Field) ->
    Field2 = Field#db_field{
               select_prevented = PreventSelect
              },
    {ok, Field2};
field_option(_Option, _Val, _Field) ->
    false.

normalize_field(Field) ->
    Rules = [
             fun default_alias_name_rule/1,
             fun alias_quoted_rule/1
            ],
    tq_transform_utils:error_writer_foldl(fun(R, F) -> R(F) end, Field, Rules).

set_field(Field, #db_model{fields=Fields} = Model) ->
    Model#db_model{fields=[Field | Fields]}.

%% Meta.

meta_clauses(#db_model{stores_in_db=true} = Model) ->
    tq_sqlmodel_generator:meta_clauses(Model);
meta_clauses(_) -> [].


%% Model rules.

default_pool_name_rule(#db_model{pool_name=undefined}=Model) ->
    Model2 = Model#db_model{pool_name = db},
    {ok, Model2};
default_pool_name_rule(Model) ->
    {ok, Model}.

stores_in_db_rule(#db_model{table=Table, fields=Fields}=Model) ->
    DbFields = lists:reverse([F || F <- Fields, F#db_field.type =/= undefined]),
    case {DbFields =/= [], Table =/= undefined} of
        {true, true} ->
            Model2 = Model#db_model{fields=DbFields, stores_in_db=true},
            {ok, Model2};
        {false, true} ->
            {error, "No db fields defined"};
        {true, false} ->
            {error, "Table name required"};
        {false, false} ->
            Model2 = Model#db_model{fields=[], stores_in_db = false},
            {ok, Model2}
    end.

table_quoted_rule(#db_model{stores_in_db=true, table=Table}=Model) ->
    case is_quated(Table) of
        true ->
            {ok, Model};
        false ->
            Model2 = Model#db_model{table=quote(Table)},
            {ok, Model2}
    end;
table_quoted_rule(Model) ->
    {ok, Model}.

%% Function names

fill_names(List, FunNames) ->
    Fun =
        fun({Name, Value}, Fns) ->
                case Name of
                    get -> Fns#funs{get = Value};
                    save -> Fns#funs{save = Value};
                    find -> Fns#funs{find = Value};
                    delete -> Fns#funs{delete = Value};
                    _ -> Fns
                end
        end,
    lists:foldl(Fun, FunNames, List).

%% Field rules.

default_alias_name_rule(#db_field{name=Name, alias=undefined}=Field) ->
    Field2 = Field#db_field{alias=list_to_binary(atom_to_list(Name))},
    {ok, Field2};
default_alias_name_rule(Field) ->
    {ok, Field}.


alias_quoted_rule(#db_field{alias=Alias}=Field) ->
    case is_quated(Alias) of
        true ->
            {ok, Field};
        false ->
            Field2 = Field#db_field{alias=quote(Alias)},
            {ok, Field2}
    end.

%% Internal functions.

quote(A) ->
    <<"\"", A/binary, "\"">>.

is_quated(A) ->
    Size = byte_size(A) - 2,
    case Size >= 0 of
        true ->
            <<First:1/binary, _:Size/binary, Last:1/binary>> = A,
            case {First, Last} of
                {$", $"} ->
                    true;
                _ ->
                    false
            end;
        false ->
            false
    end.

to_list(A) when is_list(A) ->
    A;
to_list(A) -> [A].
