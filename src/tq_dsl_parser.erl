-module(tq_dsl_parser).

-export([
         parse/3
        ]).

-record(state, {
          brace_mode=false,
          braces_cnt=0,
          joiner,
          pos=0
         }).

-define(INC_BR(S, I), S#state{braces_cnt=S#state.braces_cnt+I}).
-define(INC_POS(S, I), S#state{pos=S#state.pos+I}).

parse(Bin, Join, Init) ->
    parse(Bin, <<>>, 1, Init, #state{joiner=Join}).

parse(<<$\\, Ch, Rest/binary>>, StrAcc, StrPos, Acc, State) ->
    IsSpec =
        Ch =:= $@ orelse
        Ch =:= $# orelse
        Ch =:= $$ orelse
        Ch =:= $~,
    case IsSpec of
        true ->
            parse(Rest, <<StrAcc/binary, Ch>>, StrPos, Acc, ?INC_POS(State, 2));
        _ ->
            parse(Rest, <<StrAcc/binary, $\\, Ch>>, StrPos, Acc, ?INC_POS(State, 2))
    end;
parse(<<$(, Rest/binary>>, StrAcc, StrPos, Acc, #state{brace_mode=true, braces_cnt=Cnt}=State) ->
    State2 = State#state{pos=State#state.pos+1, braces_cnt=Cnt+1},
    parse(Rest, <<StrAcc/binary, $(>>, StrPos, Acc, State2);
parse(<<$), Rest/binary>>, StrAcc, StrPos, Acc, #state{brace_mode=true, braces_cnt=Cnt}=State) ->
    State2 = State#state{pos=State#state.pos+1, braces_cnt=Cnt-1},
    case Cnt of
        1 ->
            parse(Rest, StrAcc, StrPos, Acc, State2#state{brace_mode=false});
        _ ->
            parse(Rest, <<StrAcc/binary, $)>>, StrPos, Acc, State2)
    end;
parse(<<Quote, Rest/binary>>, StrAcc, StrPos, Acc, State) when
      Quote =:= $" orelse
      Quote =:= $' ->
    parse_string(
      Quote,
      Rest, State,
      fun(String, Rest2, State2) ->
              parse(Rest2, <<StrAcc/binary, Quote, String/binary, Quote>>, StrPos, Acc, ?INC_POS(State2, 1))
      end);
parse(<<$$, Rest/binary>>, StrAcc, StrPos, Acc, #state{joiner=Join}=State) ->
    parse_alias(
      Rest, ?INC_POS(State, 1),
      fun(Alias, Rest2, State2) ->
              Join({string, StrPos, StrAcc}, Acc,
                   fun(Acc2) ->
                           Join(Alias, Acc2,
                                fun(Acc3) ->
                                        parse(Rest2, <<>>, State2#state.pos+1, Acc3,State2)
                                end)
                   end)
      end);
parse(<<$~, Rest/binary>>, StrAcc, StrPos, Acc, #state{joiner=Join}=State) ->
    parse_type(
      Rest, ?INC_POS(State, 1),
      fun(ValueLink, Rest2, State2) ->
              Join({string, StrPos, StrAcc}, Acc,
                   fun(Acc2) ->
                           Join(ValueLink, Acc2,
                                fun(Acc3) ->
                                        parse(Rest2, <<>>, State2#state.pos+1, Acc3, State2)
                                end)
                   end)
      end);
parse(<<$#, Rest/binary>>, StrAcc, StrPos, Acc, #state{joiner=Join}=State) ->
    parse_table(
      Rest, ?INC_POS(State, 1),
      fun(Table, Rest2, State2) ->
              Join({string, StrPos, StrAcc}, Acc,
                   fun(Acc2) ->
                           Join(Table, Acc2,
                                fun(Acc3) ->
                                        parse(Rest2, <<>>, State2#state.pos+1, Acc3, State2)
                                end)
                   end)
      end);
parse(<<$@, _Rest/binary>>, _StrAcc, _StrPos, _Acc, #state{brace_mode=true}=State) ->
    {error, {wrong_format, {State#state.pos+1, "Field quering not allowed in another field quering"}}};
parse(<<$@, Rest/binary>>, StrAcc, StrPos, Acc, #state{joiner=Join}=State) ->
    parse_fq(
      Rest, ?INC_POS(State, 1),
      fun(Qf, Rest2, State2) ->
              Join({string, StrPos, StrAcc}, Acc,
                   fun(Acc2) ->
                           Join(Qf, Acc2,
                                fun(Acc3) ->
                                        parse(Rest2, <<>>, State2#state.pos+1, Acc3, State2)
                                end)
                   end)
      end);
parse(<<Ch, Rest/binary>>, StrAcc, StrPos, Acc, State) ->
    parse(Rest, <<StrAcc/binary, Ch>>, StrPos, Acc, ?INC_POS(State, 1));
parse(<<>>, _StrAcc, _StrPos, _Acc, #state{brace_mode=true}=State) ->
    {error, {wrong_format, {State#state.pos+1, "Unclosed brace )"}}};
parse(<<>>, StrAcc, StrPos, Acc, #state{joiner=Join}) ->
    Join({string, StrPos, StrAcc}, Acc,
         fun(Acc2) ->
                 Join(finish, Acc2,
                      fun(Acc3) ->
                              {ok, Acc3}
                      end)
         end).

token(Bin, State, Next) ->
    token(Bin, <<>>, State, Next).
token(<<Ch, Rest/binary>>, Acc, State, Next) when
      (Ch >= $A andalso Ch =< $Z) orelse
      (Ch >= $a andalso Ch =< $z) orelse
      (Ch >= $0 andalso Ch =< $9) orelse
      Ch =:= $_
      ->
    token(Rest, <<Acc/binary, Ch>>, ?INC_POS(State, 1), Next);
token(Rest, Acc, State, Next) ->
    Next(Acc, Rest, State).

parse_table(Bin, State, Next) ->
    token(Bin, State,
          fun(<<>>, _Rest, State2) ->
                  {error, {wrong_format, {State2#state.pos+1, "Table model name required"}}};
             (Token, Rest, State2) ->
                  Next({table, State#state.pos+1, Token}, Rest, State2)
          end).

parse_type(<<$[, Rest/binary>>, State, Next) ->
    token(Rest, State,
          fun(Type, <<$], Rest2/binary>>, State2) ->
                  Next({type, State#state.pos+2, Type}, Rest2, ?INC_POS(State2, 1));
             (_Type, _Rest2, State2) ->
                  {error, {wrong_format, {State2#state.pos+2, "Unclosed brace ]"}}}
          end);
parse_type(Bin, State, Next) ->
    token(Bin, State,
          fun(<<>>, _Rest, State2) ->
                  {error, {wrong_format, {State2#state.pos+1, "Model name required"}}};
             (Token, <<$., Rest/binary>>, State2) ->
                  token(Rest, State2,
                        fun(<<>>, _Rest2, State3) ->
                                {error, {wrong_format, {State3#state.pos+1, "Field name required"}}};
                           (Token2, Rest2, State3) ->
                                Next({field_type, State#state.pos+1, {Token, Token2}}, Rest2, ?INC_POS(State3, 1))
                        end);
             (Token, Rest, State2) ->
                  Next({field_type, State#state.pos+1, Token}, Rest, State2)
          end).

parse_alias(<<${, Rest/binary>>, State, Next) ->
    token(Rest, ?INC_POS(State, 1),
          fun(<<>>, _Rest2, _State2) ->
                  {error, {wrong_format, {State#state.pos+1, "Link name required"}}};
             (TableLink, <<$}, Rest2/binary>>, State2) ->
                  parse_alias_(Rest2, ?INC_POS(State2, 1),
                               fun(Qf, Rest3, State3) ->
                                       Next({table_link, State#state.pos+2, TableLink, Qf}, Rest3, State3)
                               end);
             (_TableLink, _Rest2, State2) ->
                  {error, {wrong_format, {State2#state.pos+1, "Close brace '}' required"}}}
          end);
parse_alias(Bin, State, Next) ->
    parse_alias_(Bin, State, Next).

parse_alias_(<<$*, Rest/binary>>, State, Next) ->
    Next({field_alias, State#state.pos+1, '*'}, Rest, ?INC_POS(State, 1));
parse_alias_(Bin, State, Next) ->
    token(Bin, State,
          fun(<<>>, _Rest, State2) ->
                  {error, {wrong_format, {State2#state.pos+1, "Model name required"}}};
             (Model, <<$., Rest/binary>>, State2) ->
                  case Rest of
                      <<$*, Rest2/binary>> ->
                          Next({field_alias, State#state.pos+1, {Model, '*'}}, Rest2, ?INC_POS(State2, 2));
                      _ ->
                          token(Rest, State2,
                                fun(<<>>, _Rest2, State3) ->
                                        {error, {wrong_format, {State3#state.pos+2, "Field name required"}}};
                                   (Field, Rest2, State3) ->
                                        Next({field_alias, State#state.pos+1, {Model, Field}}, Rest2, ?INC_POS(State3, 1))
                                end)
                  end;
             (Field, Rest, State2) ->
                  Next({field_alias, State#state.pos+1, Field}, Rest, State2)
          end).

parse_string(Quote, Bin, State, Next) ->
    parse_string(Quote, Bin, <<>>, State, Next).
parse_string(Quote, <<$\\, Ch, Rest/binary>>, Acc, State, Next) ->
    parse_string(Quote, Rest, <<Acc/binary, $\\, Ch>>, ?INC_POS(State, 2), Next);
parse_string(Quote, <<Quote, Rest/binary>>, Acc, State, Next) ->
    Next(Acc, Rest, ?INC_POS(State, 1));
parse_string(Quote, <<Ch, Rest/binary>>, Acc, State, Next) ->
    parse_string(Quote, Rest, <<Acc/binary, Ch>>, ?INC_POS(State, 1), Next);
parse_string(_Quote, <<>>, _Acc, State, _Next) ->
    {error, {wrong_format, {State#state.pos+1, "Unclosed string quate"}}}.

parse_fq(<<${, Rest/binary>>, State, Next) ->
    token(Rest, ?INC_POS(State, 1),
          fun(<<>>, _Rest2, _State2) ->
                  {error, {wrong_format, {State#state.pos+1, "Link name required"}}};
             (TableLink, <<$}, Rest2/binary>>, State2) ->
                  parse_fq_(Rest2, ?INC_POS(State2, 1),
                            fun(Qf, Rest3, State3) ->
                                    Next({table_link, State#state.pos+2, TableLink, Qf}, Rest3, State3)
                            end);
             (_TableLink, _Rest2, State2) ->
                  {error, {wrong_format, {State2#state.pos+1, "Unclosed brace '}'"}}}
          end);
parse_fq(Bin, State, Next) ->
    parse_fq_(Bin, State, Next).

parse_fq_(<<$*, Rest/binary>>, State, Next) ->
    Next({field_query_alias, State#state.pos+1, '*'}, Rest, ?INC_POS(State, 1));
parse_fq_(<<"...", Rest/binary>>, State, Next) ->
    Next({field_query_alias, State#state.pos+1, '...'}, Rest, ?INC_POS(State, 3));
parse_fq_(Bin, State, Next) ->
    GoNext =
        fun(FQ, <<$(, R/binary>>, S) ->
                S2 = S#state{pos=S#state.pos+1, braces_cnt=1, brace_mode=true},
                Next(erlang:insert_element(1, FQ, field_query), R, S2);
           (FQ, R, S) ->
                Next(erlang:insert_element(1, FQ, field_query_alias), R, S)
        end,
    token(Bin, State,
          fun(<<>>, _Rest, State2) ->
                  {error, {wrong_format, {State2#state.pos+1, "Model name required"}}};
             (Model, <<$., Rest/binary>>, State2) ->
                  case Rest of
                      <<$*, Rest2/binary>> ->
                          Next({field_query_alias, State#state.pos+1, {Model, '*'}}, Rest2, ?INC_POS(State2,2));
                      <<"..", Rest2/binary>> ->
                          Next({field_query_alias, State#state.pos+1, {Model, '...'}}, Rest2,  ?INC_POS(State2,3));
                      _ ->
                          token(Rest, State2,
                                fun(<<>>, _Rest2, State3) ->
                                        {error, {wrong_format, {State3#state.pos+2, "Field name required"}}};
                                   (Field, Rest2, State3) ->
                                        GoNext({State#state.pos+1, {Model, Field}}, Rest2,  ?INC_POS(State3,1))
                                end)
                  end;
             (Field, Rest, State2) ->
                  GoNext({State#state.pos+1, Field}, Rest, State2)
          end).

%% =============================================================================
%%% Tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_joiner(finish, Acc, Next) ->
    Next(lists:reverse(Acc));
simple_joiner(Node, Acc, Next) ->
    Next([Node|Acc]).

simple_parse(Data) ->
    parse(Data, fun simple_joiner/3, []).

is_empty_string({string, _, <<>>}) -> true;
is_empty_string(_) -> false.

parsers_test_() ->
    Tests =
        [
         {<<"#table">>, [{table, 2, <<"table">>}]},

         {<<"$field">>, [{field_alias, 2, <<"field">>}]},
         {<<"$model.field">>, [{field_alias, 2, {<<"model">>, <<"field">>}}]},

         {<<"$*">>, [{field_alias, 2, '*'}]},
         {<<"$model.*">>, [{field_alias, 2, {<<"model">>, '*'}}]},

         {<<"@*">>, [{field_query_alias, 2, '*'}]},
         {<<"@...">>, [{field_query_alias, 2, '...'}]},
         {<<"@field">>, [{field_query_alias, 2, <<"field">>}]},
         {<<"@model.field">>, [{field_query_alias, 2, {<<"model">>, <<"field">>}}]},
         {<<"@model.*">>, [{field_query_alias, 2, {<<"model">>, '*'}}]},
         {<<"@model...">>, [{field_query_alias, 2, {<<"model">>, '...'}}]},
         {<<"@field(123)">>, [{field_query, 2, <<"field">>},
                              {string, 8, <<"123">>}]},
         {<<"@model.field(123)">>, [{field_query, 2, {<<"model">>, <<"field">>}},
                                    {string, 14, <<"123">>}]},
         {<<"~field">>, [{field_type, 2, <<"field">>}]},
         {<<"~model.field">>, [{field_type, 2, {<<"model">>, <<"field">>}}]},

         {<<"~[type]">>, [{type, 3, <<"type">>}]}
        ],
    F = fun(D, R) ->
                {ok, Parsed} = simple_parse(D),
                ?assertEqual(R, [F || F <- Parsed,
                                      not is_empty_string(F)])
        end,
    [{binary_to_list(D), fun() -> F(D, R) end} || {D, R} <- Tests].

table_link_test_() ->
    Tests =
        [
         {<<"${tab}field">>, [{table_link, 3, <<"tab">>, {field_alias, 7, <<"field">>}}]},
         {<<"${tab}model.field">>, [{table_link, 3, <<"tab">>, {field_alias, 7, {<<"model">>, <<"field">>}}}]},
         {<<"${tab}*">>, [{table_link, 3, <<"tab">>, {field_alias, 7, '*'}}]},
         {<<"${tab}model.*">>, [{table_link, 3, <<"tab">>, {field_alias, 7, {<<"model">>, '*'}}}]},

         {<<"@{tab}*">>, [{table_link, 3, <<"tab">>, {field_query_alias, 7, '*'}}]},
         {<<"@{tab}...">>, [{table_link, 3, <<"tab">>, {field_query_alias, 7, '...'}}]},
         {<<"@{tab}field">>, [{table_link, 3, <<"tab">>, {field_query_alias, 7, <<"field">>}}]},
         {<<"@{tab}model.field">>, [{table_link, 3, <<"tab">>, {field_query_alias, 7, {<<"model">>, <<"field">>}}}]},
         {<<"@{tab}model.*">>, [{table_link, 3, <<"tab">>, {field_query_alias, 7, {<<"model">>, '*'}}}]},
         {<<"@{tab}model...">>, [{table_link, 3, <<"tab">>, {field_query_alias, 7, {<<"model">>, '...'}}}]},
         {<<"@{tab}field(123)">>, [{table_link, 3, <<"tab">>, {field_query, 7, <<"field">>}},
                                   {string, 13, <<"123">>}]},
         {<<"@{tab}model.field(123)">>, [{table_link, 3, <<"tab">>, {field_query, 7, {<<"model">>, <<"field">>}}},
                                         {string, 19, <<"123">>}]}
        ],
    F = fun(D, R) ->
                {ok, Parsed} = simple_parse(D),
                ?assertEqual(R, [F || F <- Parsed,
                                      not is_empty_string(F)])
        end,
    [{binary_to_list(D), fun() -> F(D, R) end} || {D, R} <- Tests].

qf_inner_test_() ->
    Tests =
        [
         {<<"@field($field2)">>, [{field_query, 2, <<"field">>},
                                  {field_alias, 9, <<"field2">>}]},
         {<<"@field(#model)">>, [{field_query, 2, <<"field">>},
                                 {table, 9, <<"model">>}]},
         {<<"@field(~field2)">>, [{field_query, 2, <<"field">>},
                                  {field_type, 9, <<"field2">>}]},
         {<<"@field(~[string])">>, [{field_query, 2, <<"field">>},
                                    {type, 10, <<"string">>}]},
         {<<"@field($field2*~[string])">>, [{field_query, 2, <<"field">>},
                                            {field_alias, 9, <<"field2">>},
                                            {string, 15, <<"*">>},
                                            {type, 18, <<"string">>}]}
        ],
    F = fun(D, R) ->
                {ok, Parsed} = simple_parse(D),
                ?assertEqual(R, [F || F <- Parsed,
                                      not is_empty_string(F)])
        end,
    [{binary_to_list(D), fun() -> F(D, R) end} || {D, R} <- Tests].

errors_test_() ->
    Tests =
        [
         {<<"$...">>, 2},
         {<<"@m. ">>, 4},
         {<<"@ ">>, 2},
         {<<"#...">>, 2},
         {<<"#*">>, 2},
         {<<"#{tab}">>, 2},
         {<<"@{tab}">>, 7},
         {<<"${tab}">>, 7},
         {<<"@{tab}f(~f2">>, 12},
         {<<"@{tab}f(~f2*(12+12)">>, 20},
         {<<"@{tab}f(@f2)">>, 9},
         {<<"~[">>, 3},
         {<<"~{">>, 2},
         {<<"~{tab}">>, 2},
         {<<"~[test">>, 7},
         {<<"${tab">>, 6}
        ],
    F = fun(D, R) ->
                {error, {wrong_format, {Pos, _Text}}} = simple_parse(D),
                ?assertEqual(R, Pos)
        end,
    [{binary_to_list(D), fun() -> F(D, R) end} || {D, R} <- Tests].

mf('...') -> <<"...">>;
mf('*') -> <<"*">>;
mf(Bin) when is_binary(Bin) -> Bin;
mf({M, '...'}) -> mf({M, <<"...">>});
mf({M, '*'}) -> mf({M, <<"*">>});
mf({M, F}) -> <<M/binary, $., F/binary>>.

h({string, Pos, Bin}) ->
    [{Pos, byte_size(Bin), Bin}];
h({table_link, Pos, Bin, Node}) ->
    [{Pos, byte_size(Bin), Bin}, h(Node)];
h({_, Pos, MF}) ->
    Bin = mf(MF),
    [{Pos, byte_size(Bin), Bin}].

g(Str, From, Size) ->
    From2 = From-1,
    <<_:From2/binary, R:Size/binary, _/binary>> = Str,
    R.

pos_big_test_() ->
    Data =
        <<"SELECT "
          "@f1, @f2, @m.f3, @{a}f4, @{a}m.f5, @f6(d1), @m.f7(d2), @..., @{t}*"
          "@{t}m.f8($f8 * (1 + $f9)), @f10(\\$f10), \\$f11, ${t}m.f12, "
          "'@m', \"@m.f12\"",
          "FROM #m AS t WHERE $f9 = ~[number]">>,
    {ok, Result} = simple_parse(Data),
    Tests = lists:flatten([h(R) || R <- Result, not is_empty_string(R)]),
    RmBrace = fun(D) -> re:replace(D, "\\)", <<>>, [global, {return, binary}]) end,
    RmSlash = fun(D) -> re:replace(D, "\\\\", <<>>, [global, {return, binary}]) end,
    Name = fun(P, S, D) -> lists:flatten(io_lib:format("~p:~p ~s", [P, S, D])) end,
    [ {Name(P, S, D),
       fun() ->
               D2 = RmSlash(RmBrace(D)),
               G = RmSlash(RmBrace(g(Data, P, S))),
               Size = byte_size(G),
               <<D3:Size/binary, _/binary>> = D2,
               ?assertEqual(G, D3)
       end} || {P, S, D} <- Tests ].

-endif.
