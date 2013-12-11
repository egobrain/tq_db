-module(db_rename).

-compile({parse_transform, tq_sqlmodel_transform}).

-field({id,
        [
         index,
         required,
         {type, integer},
         {db_type, number}
        ]}).

-field({name,
        [
         required,
         {type, binary},
         {db_type, string}
        ]}).

-model([{table, <<"simple_table">>},
        {generate, [get, save, find, delete]},
        {rename,
         [
          {get, new_get},
          {save, new_save},
          {find, new_find},
          {delete, new_delete}
         ]}
       ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

check_funs_test() ->
    Check = fun(F, A) -> erlang:function_exported(?MODULE, F, A) end,
    ?assertEqual(false, Check(get, 1)),
    ?assertEqual(false, Check(save, 1)),
    ?assertEqual(false, Check(find, 2)),
    ?assertEqual(false, Check(delete, 1)),

    ?assertEqual(Check(new_get, 1), true),
    ?assertEqual(Check(new_save, 1), true),
    ?assertEqual(Check(new_find, 2), true),
    ?assertEqual(Check(new_delete, 1), true).

-endif.
