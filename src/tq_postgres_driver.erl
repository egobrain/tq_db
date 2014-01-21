-module(tq_postgres_driver).

-behavior(tq_sql).

-include_lib("epgsql/include/pgsql.hrl").

-export([connect/1, query/3]).

connect(Args) ->
    Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    pgsql:connect(Hostname, Username, Password,
                  [
                   {database, Database}
                  ]).

'query'(Conn, Sql, Args) ->
    case escape_args(Args) of
        {ok, Args2} ->
            case pgsql:equery(Conn, Sql, Args2) of
                {ok, _Columns, Rows} ->
                    {ok, Rows};
                {ok, Count} ->
                    {ok, Count};
                {ok, Count, _Columns, Rows} ->
                    {ok, Count, Rows};
                {error, Reason} ->
                    Reason2 = transform_error(Reason),
                    {error, Reason2}
            end;
        {error, _} = Err ->
            Err
    end.

transform_error(#error{code = <<"23505">>}) ->
    not_unique;
transform_error(Error) ->
    {db_error,
     [
      {code, Error#error.code},
      {message, Error#error.message},
      {extra, Error#error.extra}
     ]}.

escape_args(Args) ->
    tq_transform_utils:error_writer_map(fun escape_arg/1, Args).

escape_arg({Name, Arg}) when
      Name =:= smallint;
      Name =:= int2;
      Name =:= integer;
      Name =:= int4;
      Name =:= bigint;
      Name =:= int8
      ->
    case Arg of
        Bin when is_binary(Bin) ->
            try
                {ok, binary_to_integer(Bin)}
            catch _ ->
                    {error, bad_arg}
            end;
        Int when is_integer(Int) ->
            {ok, Int};
        _ ->
            %% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid integer\"}]", [Arg]),
            {error, bad_arg}
    end;
escape_arg({Name, Arg}) when
      Name =:= real;
      Name =:= float4;
      Name =:= float8 ->
    case Arg of
        Bin when is_binary(Bin) ->
            try
                {ok, binary_to_integer(Bin)}
            catch _ ->
                    try
                        {ok, binary_to_float(Bin)}
                    catch _ ->
                            {error, bad_arg}
                    end
            end;
        Int when is_integer(Int) ->
            {ok, Int};
        Float when is_float(Float) ->
            {ok, Float};
        _ ->
            %% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid number\"}]", [Arg]),
            {error, bad_arg}
    end;
escape_arg({Name, Arg}) when
      Name =:= string;
      Name =:= text;
      Name =:= varchar ->
    case Arg of
        Bin when is_binary(Bin) ->
            {ok, Bin};
        _ ->
            %% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid number\"}]", [Arg]),
            {error, bad_arg}
    end;
escape_arg({datetime, Arg}) ->
    case Arg of
        {{_Y, _M, _D}, {_Hh, _Mm, _Ss}} ->
            {ok, Arg};
        _ ->
            %% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid date\"}]", [Arg]),
            {error, bad_arg}
    end;
escape_arg({Name, Arg}) when
      Name =:= boolean;
      Name =:= bool ->
    case Arg of
        true ->
            {ok, true};
        false ->
            {ok, false};
        _ ->
            %% ?ERR("BD Error [{arg, ~p}, {reason, \"must be valid boolean\"}]", [Arg]),
            {error, bad_arg}
    end.
