-module(search_engine_indexer).
-compile(export_all).
-behavior(gen_server).
-export([init/1, start_link/0, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link()->
	 gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([])->
    	State = ets:new(word_index, [ordered_set, public, {keypos, 1}]),
	{ok, State, 0}.

handle_info(timeout, State)->
	 {noreply, State};

handle_info(_, State)->
	 {noreply, State}.

handle_cast({index, Url, Response}, State)->
	error_logger:info_msg("Indexing ~p ~n", [Url]),
	{ok, SpecialCharFilter}= re:compile("[^aA-zZ0-9]"),
	SplitWords = lists:map(
				fun(X) -> binary_to_list(X) end,
				re:split(Response, SpecialCharFilter)
		 	),

	DirtyWords = lists:filter(fun(X) -> X =/= [] end, SplitWords),
	Words = lists:map(fun(X)-> string:strip(string:strip(X, both, $\n), both, $.) end, DirtyWords),
	lists:foreach(fun(X) -> 
			 case ets:member(State, X) of
			 	true ->
					{_, V} = hd(ets:lookup(State, X)),
					case lists:keysearch(Url, 2, V) of
						{value, {Count, Url}} ->
							V1 = lists:keyreplace(Url, 2, V, {Count+1, Url}),
							ets:update_element(State, X, {2, V1});
						false ->
							ets:update_element(State, X, {2, [{1, Url}|V]})
					end;
				false ->
					ets:insert(State, {X, [{1, Url}]})
			 end
		      end, Words),
	error_logger:info_msg("Indexed ~p ~n", [Url]),
	{noreply, State};

handle_cast(show_indexed, State)->
	Tab = ets:tab2list(State),
	error_logger:info_msg("Table is ~p ~n", [Tab]),
	{noreply, State};

handle_cast(_, State)-> 
	{noreply, State}.

handle_call({lookup, Word}, _, State) -> 
	Res = ets:lookup(State, Word),
	Result = case Res of
		[] -> [];
		_ -> 
			{_, V} = hd(Res),
			lists:reverse(lists:keysort(1, V))
	end,
	{reply, Result, State};

handle_call(_, _, State)->
 	{reply, State, State}.

terminate(Reason, State)->
 	{ok, Reason, State}.

code_change(_OldVersion, State, _Extra)-> 
	{ok, State}.
