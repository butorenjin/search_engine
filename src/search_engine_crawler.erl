-module(search_engine_crawler).
-behavior(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([show_indexed/0, start_link/0, fetch_results/1]).


%% Exported functions

%% @doc fetch the results corresponding to the word.
fetch_results(Word)->
	 gen_server:call(search_engine_query, {search_query, Word}).

%% @doc show all the words indexed with the URLs 
show_indexed() ->
	 gen_server:cast(search_engine_indexer, show_indexed).

start_link()->
	 gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Callback functions
init([])->
	error_logger:info_msg("Starting crawler~n"),
	{ok, {["http://example.com:8080"],[]}, 0}.

handle_info(timeout, State)->
	gen_server:cast(?MODULE, crawl),
	{noreply, State};

handle_info(_, State)->
	 {noreply, State}.

handle_cast(crawl, State={[], _})->
	{noreply, State, 10};

handle_cast(crawl, {[Url|T], Crawled})->
	case lists:member(Url, Crawled) of
		true ->
			NewState = {T, Crawled};
		false ->
			case httpc:request(Url) of 
				{ok, {_, _, Response}} ->
					gen_server:cast(search_engine_indexer, {index, Url, Response}),
					Links = parse_links(Url, Response),
					NewState = {lists:foldl(fun(Elem, Acc) -> [Elem|Acc] end, T, Links), [Url|Crawled]};
				_ ->
					NewState = {T, Crawled}
			end
	end,
	{noreply, NewState, 10};

handle_cast(_, State)->
	{noreply, State}.

accumulate(_, _, [], Acc)->  Acc;
accumulate(Url, Response, [[{Start, Length}]|T], Acc)->
	Substr = string:substr(Response, Start+1, Length),
	case string:substr(Substr,1,4) of
		"http" -> 
			accumulate(Url, Response, T, [Substr|Acc]);
		_ ->
			accumulate(Url, Response, T, [Url ++ Substr|Acc])
	end.

parse_links(Url, Response) ->
	{ok, MP} = re:compile("href\s*\=\s*[\"'](?P<URL>[^\"']*)"),
	case re:run(Response, MP, [{capture, ['URL']}, global]) of
		{match, Matches} ->
			accumulate(Url, Response, Matches, []);
		_ ->
			[]
	end.

handle_call(_, _, State)->
	 {reply, State}.

terminate(Reason, State)->
	 {ok, Reason, State}.

code_change(_Oldversion, State, _Extra)->
	 {ok, State}.
