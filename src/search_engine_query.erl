-module(search_engine_query).
-compile(export_all).
-behavior(gen_server).
-export([init/1, start_link/0, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([])->
	{ok, [], 0}.

handle_info(timeout, State)->
	error_logger:info_msg("Timeout! ~n"),
	{noreply, State};

handle_info(_, State) ->
	{noreply, State}.

handle_cast(_, State)->
	{noreply, State}.

handle_call({search_query,Q}, _, State)->
	Words = string:tokens(Q, " "),
	{reply, lookup(Words), State};

handle_call(_, _, State)->
	{reply, ok, State}.

terminate(Reason, State)->
	{ok, Reason, State}.

code_change(_Oldversion, State, _Extra)->
	{ok, State}.

lookup(X) ->
	lookup(X, []).

lookup([], Acc) ->
	 Acc;

lookup([H|Words], Acc)->
	 Res = gen_server:call(search_engine_indexer, {lookup, H}),
	 lookup(Words, [Res|Acc]).

