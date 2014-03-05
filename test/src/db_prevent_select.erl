-module(db_prevent_select).

-compile({parse_transform, tq_sqlmodel_transform}).

-field({id,
        [
         index,
         {type, non_neg_integer},
         {db_type, integer}
        ]}).

-field({counter,
        [
         index,
         {type, non_neg_integer},
         {db_type, integer}
        ]}).


-field({note,
        [
         index,
         {type, binary},
         {db_type, varchar},
         prevent_select
        ]}).

-model([
        {table, <<"test">>}
       ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prevent_test() ->
    [id, counter] = '$meta'({db_fields, r}),
    <<"\"note\"">> = '$meta'({db_alias, note}),
    varchar = '$meta'({db_type, note}),
    <<"data">> = ((constructor([note]))([<<"data">>])):note().

-endif.
