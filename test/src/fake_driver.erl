-module(fake_driver).

-export([
         start_pool/2,

         'query'/4,
         'parse'/2,

         find/4,
         get/3,
         insert/3,
         update/4,
         delete/3
        ]).

%% =============================================================================
%% API functions
%% =============================================================================

start_pool(_PoolName, _Opts) ->
    ok.

'query'(_PoolName, _Sql, _Args, _Constructor) ->
    throw(not_implemented).

parse(_Query, _QueryArgs) ->
    throw(not_implemented).

find(_PoolName, _Module, _Query, _QueryArgs) ->
    throw(not_implemented).

get(_PoolName, _Module, _IndexFV) ->
    throw(not_implemented).

insert(_PoolName, Module, ChangedFV) ->
    {Fields, Values} = lists:unzip(ChangedFV),
    Constructor = Module:constructor(Fields),
    Model = Constructor(Values),
    {ok, Model}.

update(_PoolName, Module, ChangedFV, IndexFV) ->
    {Fields1, Values1} = lists:unzip(ChangedFV),
    {Fields2, Values2} = lists:unzip(IndexFV),
    Constructor = Module:constructor(Fields1 ++ Fields2),
    Model = Constructor(Values1 ++ Values2),
    {ok, Model}.

delete(_PoolName, _Module, _IndexFV) ->
    ok.
