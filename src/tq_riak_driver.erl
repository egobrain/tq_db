-module(tq_riak_driver).

-behaviour(tq_kv).

-export([connect/1,
		 put/3,
		 get/2]).

connect(Args) ->
	Hostname = proplists:get_value(hostname, Args),
	Port = proplists:get_value(port, Args, 8087),
	Bucket = proplists:get_value(bucket, Args, 8087),
	case riakc_pb_socket:start_link(Hostname, Port) of
		{ok, Conn} ->
			{ok, {Conn, Bucket}};
		{error, Reason} ->
			{error, Reason}
	end.

put({Conn, Bucket}, Key, Value) ->
	Obj = riakc_obj:new(Bucket, Key, term_to_binary(Value)),
	riakc_pb_socket:put(Conn, Obj).

get({Conn, Bucket}, Key) ->
	case riakc_pb_socket:get(Conn, Bucket, Key) of
		{ok, Obj} ->
			{ok, binary_to_term(riakc_obj:get_value(Obj))};
		_ = Err ->
			Err
	end.
