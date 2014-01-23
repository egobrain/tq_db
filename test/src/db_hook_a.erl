-module(db_hook_a).

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
        {after_save,
         [
          {after_save, [1]}
         ]},
        {after_delete,
         [
          {after_delete, [1]}
         ]}
       ]).

append(Key, Value) ->
    List = get(Key),
    put(Key, [Value|List]).

after_save(Tag, _OldModel, NewModel) ->
    append(save, {a, Tag}),
    {ok, NewModel}.

after_delete(Tag, _Model) ->
    append(delete, {a, Tag}),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

save_test() ->
    put(save, []),
    {ok, _Model} = test_utils:save(?MODULE:new()),
    ?assertEqual(lists:reverse(get(save)), [{a, 1}]).

delete_test() ->
    put(delete, []),
    ok = test_utils:delete(?MODULE:new()),
    ?assertEqual(lists:reverse(get(delete)), [{a, 1}]).

-endif.
