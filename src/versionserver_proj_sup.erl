-module(versionserver_proj_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, { {simple_one_for_one, 5, 10},
	       [{versionserver_project,
		 {versionserver_proj, start_link, []},
		 permanent, 5000, worker, [versionserver_proj]}
	       ]} }.
