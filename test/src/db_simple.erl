-module(db_simple).

-compile({parse_transform, tq_sqlmodel_transform}).

-field({index, [index,
                required,
                {type, integer},
                {db_type, number},
                {db_alias, <<"id">>}]}).

-field({name, [required,
               {type, binary},
               {db_type, string},
               {db_alias, <<"name">>}]}).

-field({tmp, [required,
              {from_ext, any},
              {type, any},
              {db_type, any}
             ]}).

-field({to_from,
        [{from_ext, any},
         {type, any},
         {db_type, any},
         {to_db, {to_db, [test]}},
         {from_db, {from_db, [test]}}
        ]}).

-model([{table, <<"simple_table">>},
        {generate, [get, save, find, delete]},

        {before_save, before_save},
        {after_save, after_save},
        {before_delete, before_delete},
        {after_delete, after_delete}
       ]).

any(A) -> A.

to_db(Tag, Value) ->
    {to_db, Tag, Value}.

from_db(Tag, Value) ->
    {from_db, Tag, Value}.

before_save(Model) ->
    {ok, Model:set_tmp({before, Model:tmp()})}.

after_save(_OldModel, NewModel) ->
    {ok, NewModel:set_tmp({'after', NewModel:tmp()})}.

before_delete(_Model) ->
    ok.
after_delete(_Model) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

before_after_test() ->
    meck:new(tq_sqlmodel_runtime, [unstick, passthrough]),
    meck:expect(tq_sqlmodel_runtime, 'save',
                fun(_, M) ->
                        {ok, M:set_tmp({save, M:tmp()})}
                end),
    {ok, Model} = ?MODULE:from_proplist([{tmp, data}]),
    {ok, Model2} = Model:save(),
    meck:unload(tq_sqlmodel_runtime),
    ?assertEqual({'after', {save, {before, data}}}, Model2:tmp()).

-endif.
