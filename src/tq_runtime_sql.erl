-module(tq_runtime_sql).

-export([
         'query'/4,
         model_query/4,
         parse/2
        ]).

model_query(PoolName, Model, RuntimeSql, Args) ->
    case parse(Model, RuntimeSql) of
        {ok, {Sql, Fields, DbTypes}} ->
            Args2 = lists:zipwith(fun(W, F) -> W(F) end, DbTypes, Args),
            tq_sql:'query'(PoolName, Sql, Args2, Model:constructor(Fields));
        {error, _Reason} = Err -> Err
    end.

'query'(PoolName, RuntimeSql, Args, Constructor) ->
    case parse(undefined, RuntimeSql) of
        {ok, {Sql, _Fields, DbTypes}} ->
            Args2 = lists:zipwith(fun(W, F) -> W(F) end, DbTypes, Args),
            tq_sql:'query'(PoolName, Sql, Args2, Constructor);
        {error, _Reason} = Err -> Err
    end.

-record(state, {
          acc = <<>>,
          args = [],
          fields = [],
          model,
          arg_index = 0
         }).

parse(Model, Bin) ->
    tq_runtime_sql_parser:parse(Bin, fun sql_joiner/3, #state{model=Model}).

sql_joiner({string, _Pos, Str}, #state{acc=Acc}=State, Next) ->
    Acc2 = <<Acc/binary, Str/binary>>,
    Next(State#state{acc=Acc2});
sql_joiner({field_query, Pos, FieldQuery}, State, Next) ->
    field_query(<<>>, FieldQuery, false, Pos, State, Next);
sql_joiner({field_query_alias, Pos, FieldQuery}, State, Next) ->
    field_query(<<>>, FieldQuery, true, Pos, State, Next);
sql_joiner({field_alias, _Pos, MF}, State, Next) ->
    field_alias(<<>>, MF, _Pos, State, Next);
sql_joiner({field_type, _Pos, MF}, #state{model=DefaultModel}=State, Next) ->
    {Model, Field} = get_model_and_field(MF, DefaultModel),
    Type = Model:'$meta'({db_type, Field}),
    TypeWrapper = fun(Val) -> {Type, Model:field_to_db(Field, Val)} end,
    arg_type(TypeWrapper, State, Next);
sql_joiner({table, _Pos, BinModel}, #state{acc=Acc}=State, Next) ->
    Model = binary_to_atom(BinModel),
    Table = Model:'$meta'(table),
    Acc2 = <<Acc/binary, Table/binary>>,
    Next(State#state{acc=Acc2});
sql_joiner({table_link, _Pos, TableLink, {field_query, Pos, FieldQuery}}, State, Next) ->
    field_query(<<$", TableLink/binary, $", $.>>, FieldQuery, false, Pos, State, Next);
sql_joiner({table_link, _Pos, TableLink, {field_query_alias, Pos, FieldQuery}}, State, Next) ->
    field_query(<<$", TableLink/binary, $", $.>>, FieldQuery, true, Pos, State, Next);
sql_joiner({table_link, _Pos, TableLink, {field_alias, _Pos2, MF}}, State, Next) ->
    field_alias(<<$", TableLink/binary, $", $.>>, MF, _Pos, State, Next);
sql_joiner({type, _Pos, BinType}, State, Next) ->
    Type = binary_to_atom(BinType),
    TypeWrapper = fun(Val) -> {Type, Val} end,
    arg_type(TypeWrapper, State, Next);
sql_joiner(finish, #state{acc=Acc, fields=Fields, args=Args}, Next) ->
    Next({Acc, lists:flatten(Fields), lists:reverse(Args)}).

get_model_and_field({BinModel, BinFields}, _DefaultModel) ->
    {binary_to_atom(BinModel), binary_to_atom(BinFields)};
get_model_and_field(BinField, DefaultModel) when is_binary(BinField) ->
    {DefaultModel, binary_to_atom(BinField)};
get_model_and_field(AtomField, DefaultModel) when is_atom(AtomField) ->
    {DefaultModel, AtomField}.

field_alias(TableLink, MF, _Pos, #state{acc=Acc, model=DefaultModel} = State, Next) ->
    {Model, Field} = get_model_and_field(MF, DefaultModel),
    Alias = Model:'$meta'({db_alias, Field}),
    State2 = State#state{
               acc = <<Acc/binary, TableLink/binary, Alias/binary>>
              },
    Next(State2).

field_query(TableLink, '...', _Expr, _Pos,
            #state{acc=Acc, fields=Fields, model=Model}=State, Next) ->
    DbFields = Model:'$meta'({db_fields, r}),
    ResultFields = DbFields -- Fields,
    FieldsSql = join_fields(TableLink, Model, ResultFields),
    State2 = State#state{
               acc = <<Acc/binary, FieldsSql/binary>>,
               fields = Fields ++ ResultFields
              },
    Next(State2);
field_query(TableLink, '*', _IsAlias, _Pos,
            #state{acc=Acc, fields=Fields, model=Model}=State, Next) ->
    DbFields = Model:'$meta'({db_fields, r}),
    FieldsSql = join_fields(TableLink, Model, DbFields),
    State2 = State#state{
               acc = <<Acc/binary, FieldsSql/binary>>,
               fields = Fields ++ DbFields
              },
    Next(State2);
field_query(TableLink, {BinModel, BinField}, IsAlias, Pos,
            #state{model=Model}=State, Next) ->
    M = binary_to_atom(BinModel),
    case M of
        Model ->
            field_query(TableLink, BinField, IsAlias, Pos, State, Next);
        _ ->
            {error, {wrong_model, {Pos, "Querying data from another model not supported"}}}
    end;
field_query(TableLink, BinField, IsAlias, _Pos,
            #state{acc=Acc, fields=Fields, model=Model}=State, Next)
  when is_binary(BinField) ->
    Field = binary_to_atom(BinField),
    State2 =
        case IsAlias of
            true ->
                FieldSql = field_to_sql(TableLink, Model, Field),
                State#state{
                  acc = <<Acc/binary, FieldSql/binary>>
                 };
            false ->
                State
        end,
    State3 = State2#state{
               fields = Fields ++ [Field]
              },
    Next(State3).

arg_type(TypeWrapper, #state{acc=Acc, args=Args, arg_index=ArgIndex} = State, Next) ->
    NewArgIndex = ArgIndex + 1,
    BinArgIndex = integer_to_binary(NewArgIndex),
    State2 =
        State#state{
          acc = <<Acc/binary, $$, BinArgIndex/binary>>,
          args = [TypeWrapper|Args],
          arg_index = NewArgIndex
         },
    Next(State2).

join_fields(_TableLink, _Model, []) ->
    <<>>;
join_fields(TableLink, Model, Fields) ->
    <<$,, Str/binary>> = << <<$,, (field_to_sql(TableLink, Model, F))/binary>> || F <- Fields >>,
    Str.

field_to_sql(TableLink, Model, Field) ->
    <<TableLink/binary, (Model:'$meta'({db_alias, Field}))/binary>>.

binary_to_atom(Bin) ->
    List = binary_to_list(Bin),
    list_to_existing_atom(List).
