# 0.5.0

## Fixes

- to_db transformation logic. No unnecessary checks animore.

## Features

- Get all model fields alias: `$*`
- Force saving unchanged fields using new `save/2` functions and `force` option.
- DSL. New fields alias construction, that returns full field name including table name.
- New function in tq_dsl, that allows to query model with custom cosntructor.
- New field option `prevent_select`. Field, marked with this option, won't be selected from table by default.

## Breaking Changes

- Simplified DSL Ast. New dsl constructions.
- Querying aliased field custom data not allowed.
