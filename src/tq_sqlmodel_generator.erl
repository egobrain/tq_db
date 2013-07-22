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
	TableClause = ?clause([?atom(table)], none,
							[?abstract(Table)]),
	DbAliasClause = [?clause([?tuple([?atom(db_alias), ?var('F')])], none,
							 [?cases(?var('F'), [?clause([?atom(F#field.name)], none,
														 [?abstract(F#field.alias)])
												 || F <- Fields])])],
	DbTypesClause = [?clause([?tuple([?atom(db_type), ?var('F')])], none,
							 [?cases(?var('F'), [?clause([?atom(F#field.name)], none,
														 [?abstract(F#field.type)])
												 || F <- Fields])])],
	DbRFieldsClaues = ?clause([?abstract({db_fields, r})], none,
							 [?list([?atom(F#field.name)
									 || F <- Fields, F#field.mode#access_mode.sr])]),
	DbWFieldsClaues = ?clause([?abstract({db_fields, w})], none,
							  [?list([?atom(F#field.name)
									  || F <- Fields, F#field.mode#access_mode.sw])]),
	[$, | SqlRFields] = lists:flatten([[$,, atom_to_quated_string(F#field.name)]
										|| F <- Fields, F#field.mode#access_mode.sr]),
	RSqlFieldsClause = ?clause([?abstract({sql, {db_fields, r}})], none,
							   [?string(SqlRFields)]),

	lists:flatten([TableClause,
				   DbTypesClause,
				   DbAliasClause,
				   DbRFieldsClaues,
				   DbWFieldsClaues,
				   RSqlFieldsClause]).

atom_to_quated_string(Atom) ->
	lists:flatten("\""++atom_to_list(Atom)++"\"").
