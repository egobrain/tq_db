%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(tq_sqlmodel_transform).

-behavior(tq_transform_plugin).

-include("include/db.hrl").

-export([parse_transform/2]).

-export([create_model/1,
		 model_option/3,
		 normalize_model/1,
		 build_model/1,

		 create_field/1,
		 field_option/3,
		 normalize_field/1,
		 set_field/2,

		 meta_clauses/1
		]).

parse_transform(Ast, Options) ->
	tq_transform:parse_transform(Ast, Options, [tq_record_transform, ?MODULE]).

%% Model.

create_model(Module) ->
	#model{module=Module}.

model_option(table, Table, Model) ->
	Model2 = Model#model{table = Table},
	{ok, Model2};
model_option(_Option, _Val, _Model) ->
	false.

normalize_model(Model) ->
	Rules = [
			 fun stores_in_db_rule/1,
			 fun table_quoted_rule/1
			],
	tq_transform_utils:error_writer_foldl(fun(R, M) -> R(M) end, Model, Rules).

build_model(_Model) ->
	{[], []}.

%% Fields.

create_field(Name) ->
	#field{name=Name}.

field_option(db_type, Type, Field) ->
	Field2 = Field#field{type = Type},
	{ok, Field2};
field_option(db_alias, Alias, Field) ->
	Field2 = Field#field{alias = Alias},
	{ok, Field2};
field_option(_Option, _Val, _Field) ->
	false.

normalize_field(Field) ->
	Rules = [
			 fun default_alias_name_rule/1,
			 fun alias_quoted_rule/1
			],
	tq_transform_utils:error_writer_foldl(fun(R, F) -> R(F) end, Field, Rules).

set_field(Field, #model{fields=Fields} = Model) ->
	Model#model{fields=[Field | Fields]}.

%% Meta.

meta_clauses(#model{stores_in_db=true} = Model) ->
	tq_db_generator:meta_clauses(Model);
meta_clauses(_) -> [].


%% Model rules.

stores_in_db_rule(#model{table=Table, fields=Fields}=Model) ->
	DbFields = lists:filter(fun(#field{type=undefined}) -> false;
							   (_) -> true
							end, Fields),
	case {DbFields =/= [], Table =/= undefined} of
		{true, true} ->
			Model2 = Model#model{stores_in_db = true},
			{ok, Model2};
		{false, true} ->
			{error, "No db fields defined"};
		{true, false} ->
			{error, "Table name required"};
		{false, false} ->
			Model2 = Model#model{stores_in_db = false},
			{ok, Model2}
	end.

table_quoted_rule(#model{stores_in_db=true, table=Table}=Model) ->
	case is_quated(Table) of
		true ->
			{ok, Model};
		false ->
			Model2 = Model#model{table=quote(Table)},
			{ok, Model2}
	end;
table_quoted_rule(Model) ->
	{ok, Model}.


%% Field rules.

default_alias_name_rule(#field{name=Name, alias=undefined}=Field) ->
	Field2 = Field#field{alias=list_to_binary(atom_to_list(Name))},
	{ok, Field2};
default_alias_name_rule(Field) ->
	{ok, Field}.


alias_quoted_rule(#field{alias=Alias}=Field) ->
	case is_quated(Alias) of
		true ->
			{ok, Field};
		false ->
			Field2 = Field#field{alias=quote(Alias)},
			{ok, Field2}
	end.

%% Internal functions.

quote(A) ->
	<<"\"", A/binary, "\"">>.

is_quated(A) ->
	Size = byte_size(A) - 2,
	case Size >= 0 of
		true ->
			<<First:1/binary, _:Size/binary, Last:1/binary>> = A,
			case {First, Last} of
				{$", $"} ->
					true;
				_ ->
					true
			end;
		false ->
			false
	end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
