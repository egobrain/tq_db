-module(db_field_to_db).

-compile({parse_transform, tq_sqlmodel_transform}).

%% Test
-field({id,
        [
         index,
         required,
         {type, integer},
         {db_type, integer}
        ]}).

-field({data,
        [
         {type, binary},
         {db_type, varchar},
         {to_db, to_db_exception}
        ]}).

-model([
        {table, <<"test">>},
        {generate, [save]}
       ]).


to_db_exception(Value) ->
    throw({to_db_exception, Value}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Save mustn't execute to_db transformations for unchanged fields
to_db_transformations_test() ->
    Model = new(),
    {ok, _Model} = Model:save().

-endif.
