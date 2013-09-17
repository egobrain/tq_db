-module(db_init).

-compile({parse_transform, tq_sqlmodel_transform}).

%% Test
-field({counter,
        [
         required,
         {type, integer},
         {db_type, integer},
         record, get, set,
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
        {from_db, [init1,
                init2]}
       ]).

init1(Model) ->
    case Model:counter() of
        undefined ->
            Model;
        Counter ->
            Model:set_counter(Counter*3)
    end.

init2(Model) ->
    case Model:counter() of
        undefined ->
            Model;
        Counter ->
            Model:set_counter(Counter*10)
    end.

field_init1(FieldVal) ->
    FieldVal*3.
field_init2(FieldVal) ->
    FieldVal*10.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_db_test_() ->
    C = fun(V) ->
                {ok, Model} = from_proplist([{counter, V}]),
                Model#?MODULE{'$is_new$'=false}
        end,
    [fun() -> ?assertEqual(C(30), (constructor([]))([])) end,
     fun() -> ?assertEqual(C(150), (constructor([counter]))([5])) end].

from_db_field_test_() ->
    G = fun(M) ->
                M:from_db_field()
        end,
    [fun() -> ?assertEqual(1, G((constructor([]))([]))) end,
     fun() -> ?assertEqual(150, G((constructor([from_db_field]))([5]))) end].

-endif.
