%% Copyright (c) 2011-2013, Jakov Kozlov <xazar.studio@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(tq_sqlmodel_generator).

-include("include/db.hrl").
-include("include/ast_helpers.hrl").

-export([meta_clauses/1]).

meta_clauses(#model{table=Table, fields=Fields}) ->
	DbTableClause = ?clause([?atom(db_table)], none,
							[?abstract(Table)]),
	DbAliasClause = [?clause([?tuple([?atom(db_alias), ?var('F')])], none,
							 [?cases(?var('F'), [?clause([?atom(F#field.name)], none,
														 [?abstract(F#field.alias)])
									  || F <- Fields, F#field.type =/= undefined])])],
	DbTypesClause = [?clause([?tuple([?atom(db_type), ?var('F')])], none,
							 [?cases(?var('F'), [?clause([?atom(F#field.name)], none,
														 [?abstract(F#field.type)])
									  || F <- Fields, F#field.type =/= undefined])])],
	lists:flatten([DbTableClause, DbTypesClause, DbAliasClause]).
