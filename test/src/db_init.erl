-module(db_init).

-compile({parse_transform, tq_sqlmodel_transform}).

%% Test
-field({counter,
        [
         index,
         required,
         {type, integer},
         {db_type, integer},
         record, get, set,
         {to_db, to_db},
         {default, 1}
        ]}).

-field({from_db_field,
        [
         required,
         {type, integer},
         {db_type, integer},
         record, get, set,
         {default, 1},
         {from_db, [field_init1,
                 field_init2]}
        ]}).

-model([
        {table, <<"test">>},
        {generate, [get, save, delete, find]},
        {from_db, [init1, init2]}
       ]).

init1(Model) ->
    case Model:counter() of
        undefined ->
            Model;
        Counter ->
            Model#?MODULE{counter = {init1, Counter}}
    end.

init2(Model) ->
    case Model:counter() of
        undefined ->
            Model;
        Counter ->
            Model#?MODULE{counter = {init2, Counter}}
    end.

field_init1(FieldVal) ->
    {field_init1, FieldVal}.
field_init2(FieldVal) ->
    {field_init2, FieldVal}.

to_db(Value) ->
    {to_db, Value}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_db_test_() ->
    C = fun(V) ->
                #?MODULE{
                    counter = V
                   }
        end,
    [fun() -> ?assertEqual(C(undefined), (constructor([]))([])) end,
     fun() -> ?assertEqual(C({init2,{init1,5}}), (constructor([counter]))([5])) end].

from_db_field_test_() ->
    G = fun(M) ->
                M:from_db_field()
        end,
    [fun() -> ?assertEqual(undefined,
                           G((constructor([]))([]))) end,
     fun() -> ?assertEqual({field_init2,{field_init1,5}},
                           G((constructor([from_db_field]))([5]))) end].

to_db_test() ->
    test_utils:fake_driver(),
    {ok, Model} = ?MODULE:from_proplist([{counter, 100}]),
    {ok, Model2} = Model:save(),
    ?assertEqual({init2,{init1,{to_db,100}}}, Model2:counter()).

-endif.
