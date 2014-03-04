-module(db_save).

-compile({parse_transform, tq_sqlmodel_transform}).

%% Test
-field({id,
        [
         index,
         {type, integer},
         {db_type, integer}
        ]}).
-field({counter,
        [
         index,
         {type, integer},
         {db_type, integer},
         {from_db, from_db},
         {to_db, to_db}
        ]}).

-model([
        {table, <<"test">>},
        {generate, [save]}
       ]).

to_db(Value) ->
    {to_db, Value}.

from_db(Value) ->
    {from_db, Value}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

save_test() ->
    test_utils:fake_driver(),
    {ok, Model} = from_proplist([{counter, 1}]),
    {ok, Model2} = Model:save(),
    {from_db, {to_db, 1}} = Model2:counter().

force_save_test() ->
    Model = (constructor([counter]))([1]),
    {error, not_modified} = Model:save(),
    {ok, Model2} = Model:save([force]),
    {from_db, {to_db, {from_db, 1}}} = Model2:counter().

-endif.
