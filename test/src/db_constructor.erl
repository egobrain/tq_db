-module(db_constructor).

-compile({parse_transform, tq_sqlmodel_transform}).

%% Test
-field({r,    [{type, integer}, {db_type, integer}, {mode, r}]}).
-field({w,    [{type, integer}, {db_type, integer}, {mode, w}]}).
-field({rw,   [{type, integer}, {db_type, integer}, {mode, rw}]}).
-field({sr,   [{type, integer}, {db_type, integer}, {mode, sr}]}).
-field({sw,   [{type, integer}, {db_type, integer}, {mode, sw}]}).
-field({srsw, [{type, integer}, {db_type, integer}, {mode, srsw}]}).
-field({rsw,  [{type, integer}, {db_type, integer}, {mode, rsw}]}).
-field({srw,  [{type, integer}, {db_type, integer}, {mode, srw}]}).

-model([{table, <<"test">>}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Opts [r, w, rw, sr, sw, srsw, rsw, srw]

constructor_fail_test_() ->
    Tests = [w, sw],
    [fun() -> ?_assertException(error, function_clause, constructor([F])) end || F <- Tests].

constructor_test_() ->
    Tests = [r, rw, sr, srsw, rsw, srw],
    [fun() ->  constructor([F]) end || F <- Tests].

constructor_changed_fields_test() ->
    Opts = [r, rw, sr, srsw, rsw, srw],
    Constructor = constructor(Opts),
    Model = Constructor(Opts),
    ?assertEqual(Model:get_changed_fields(), []).

-endif.
