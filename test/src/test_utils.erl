-module(test_utils).

-export([
         stumb_mf/3,

         save/1,
         delete/1
        ]).

stumb_mf(M, F, Do) ->
    fun(Exec) ->
            meck:new(M, [unstick, passthrough]),
            try
                meck:expect(M, F, Do),
                Res = Exec(),
                meck:unload(M),
                Res
            catch E:R ->
                    meck:unload(),
                    erlang:raise(E, R, erlang:get_stacktrace())
            end
    end.

save(Model) ->
    Do = stumb_mf(tq_sqlmodel_runtime, save, fun(_, M) -> {ok, M} end),
    Do(fun() -> Model:save() end).

delete(Model) ->
    Do = stumb_mf(tq_runtime_sql, model_query, fun(_, _, _, _) -> {ok, 1} end),
    Do(fun() -> Model:delete() end).
