%%%-------------------------------------------------------------------
%%% @author egobrain <>
%%% @copyright (C) 2013, egobrain
%%% @doc
%%%
%%% @end
%%% Created :  3 Jul 2013 by egobrain <>
%%%-------------------------------------------------------------------
-module(tq_kv_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {conn, driver}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Driver, DbArgs}) ->
	case Driver:connect(DbArgs) of
		{ok, Conn} ->
			{ok, #state{conn=Conn, driver = Driver}};
		{error, Reason} ->
			{error, Reason}
	end.

handle_call({put, Key, Value}, _From, #state{conn=Conn, driver=Driver}=State) ->
	Resp = Driver:put(Conn, Key, Value),
	{reply, Resp, State};
handle_call({get, Key}, _From, #state{conn=Conn, driver=Driver}=State) ->
	Resp = Driver:get(Conn, Key),
	{reply, Resp, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

