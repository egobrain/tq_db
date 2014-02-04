-module(tq_sqlmodel_runtime).

-export([save/2,
         success_foldl/2
        ]).

save(Changed, Model) ->
    case {Model:is_new(), Changed} of
        {true, []} ->
            {error, not_changed};
        {false, []} ->
            {ok, Model};
        {IsNew, _} ->
            Table = Model:'$meta'(table),
            RFields = Model:'$meta'({db_fields, r}),
            <<$,, Returning/binary>> =
                << <<$,, (Model:'$meta'({db_alias, F}))/binary>> || F <- RFields>>,
            Constructor = (element(1, Model)):constructor(RFields),
            Args = [{Model:'$meta'({db_type,F}),V} || {F,V} <- Changed],
            {Sql2, Args2} =
                case IsNew of
                    true ->
                        <<$,, Fields/binary>>
                            = << <<$,, (Model:'$meta'({db_alias,F}))/binary>>
                                 || {F,_} <- Changed >>,
                        {<<$,, Values/binary>>, _} =
                            each_with(
                              fun(_, I, Acc) ->
                                      BinIndex = integer_to_binary(I),
                                      <<Acc/binary, ", $", BinIndex/binary>>
                              end, <<>>, Changed),
                        Sql = <<"INSERT INTO ", Table/binary,
                                "(", Fields/binary, ")",
                                " VALUES (", Values/binary, ")",
                                " RETURNING ", Returning/binary, ";">>,
                        {Sql, Args};
                    false ->
                        Indexes = Model:'$meta'(indexes),
                        {<<$,, Values/binary>>, Cnt} =
                            each_with(
                              fun({F, _V}, I, Acc) ->
                                      Alias = Model:'$meta'({db_alias, F}),
                                      BinIndex = integer_to_binary(I),
                                      <<Acc/binary, ", ", Alias/binary, " = $", BinIndex/binary>>
                              end, <<>>, Changed),
                        {<<" AND ", Where/binary>>, _} =
                            each_with(
                              fun(F, I, Acc) ->
                                      Alias = Model:'$meta'({db_alias, F}),
                                      BinIndex = integer_to_binary(I),
                                      <<Acc/binary, " AND ", Alias/binary, " = $", BinIndex/binary>>
                              end, Cnt, <<>>, Indexes),
                        WhereArgs = [{Model:'$meta'({db_type, F}), Model:F()} || F <- Indexes],
                        Sql = <<"UPDATE ", Table/binary,
                                " SET ", Values/binary,
                                " WHERE ", Where/binary,
                                " RETURNING ", Returning/binary, ";">>,
                        {Sql, Args ++ WhereArgs}
                end,
            case tq_sql:'query'(db, Sql2, Args2, Constructor) of
                {ok, 1, [Model2]} ->
                    {ok, Model2};
                {error, _} = Err ->
                    Err
            end
    end.

each_with(Fun, Acc, List) ->
    each_with(Fun, 1, Acc, List).
each_with(_Fun, Index, Acc, []) ->
    {Acc, Index};
each_with(Fun, Index, Acc, [H|T]) ->
    Acc2 = Fun(H, Index, Acc),
    each_with(Fun, Index+1, Acc2, T).


success_foldl(Data, []) ->
    {ok, Data};
success_foldl(Data, [F|Rest]) ->
    case F(Data) of
        ok -> success_foldl(Data, Rest);
        {ok, Data2} -> success_foldl(Data2, Rest);
        {error, _Reason} = Err -> Err
    end.
