-module(tq_db_utils).

-export([
         success_foldl/2
        ]).

success_foldl(Data, []) ->
    {ok, Data};
success_foldl(Data, [F|Rest]) ->
    case F(Data) of
        ok -> success_foldl(Data, Rest);
        {ok, Data2} -> success_foldl(Data2, Rest);
        {error, _Reason} = Err -> Err
    end.
