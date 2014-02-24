-module(db_hook_b).

-compile({parse_transform, tq_sqlmodel_transform}).

-field({index,
        [
         index,
         required,
         {type, integer},
         {db_type, integer},
         {db_alias, <<"id">>}
        ]}).

-field({value,
        [
         required,
         {type, integer},
         {db_type, integer},
         {default, 100}
        ]}).

-model([
        {table, <<"simple_table">>},
        {generate, [save, delete]},
        {before_save,
         [
          {before_save, [1]}
         ]},
        {before_delete,
         [
          {before_delete, [1]}
         ]}
       ]).

append(Key, Value) ->
    List = get(Key),
    put(Key, [Value|List]).

before_save(Tag, Model) ->
    append(save, {b, Tag}),
    {ok, Model}.

before_delete(Tag, _Model) ->
    append(delete, {b, Tag}),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

save_test() ->
    put(save, []),
    test_utils:fake_driver(),
    Model = ?MODULE:new(),
    {ok, _Model} = Model:save(),
    ?assertEqual(lists:reverse(get(save)), [{b, 1}]).

delete_test() ->
    put(delete, []),
    test_utils:fake_driver(),
    Model = ?MODULE:new(),
    ok = Model:delete(),
    ?assertEqual(lists:reverse(get(delete)), [{b, 1}]).

-endif.
