
-module(search_engine_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    application:start(inets),
    application:start(mnesia),
    Indexer = ?CHILD(search_engine_indexer, worker),
    Crawler = ?CHILD(search_engine_crawler, worker),
    QueryLayer = ?CHILD(search_engine_query, worker),

    Children = [Indexer, Crawler, QueryLayer],
    {ok, { {one_for_one, 5, 10}, Children} }.

