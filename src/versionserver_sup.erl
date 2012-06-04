
-module(versionserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Shutdown), 
	{I, {I, start_link, []}, permanent, Shutdown, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, { {one_for_one, 5, 10}, 
	       [?CHILD(versionserver_serv, worker, 5000),
		?CHILD(versionserver_proj_sup, supervisor, 30000)]} }.

