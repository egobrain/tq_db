-module(test_utils).

-export([
         fake_driver/0
        ]).

fake_driver() ->
    application:load(tq_db),
    application:set_env(tq_db, pools, [{db, fake_driver, []}]),
    application:start(tq_db).
