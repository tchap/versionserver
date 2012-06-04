-module(versionserver_proj).

-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% Server callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Project) when is_atom(Project) ->
	gen_server:start_link(?MODULE, Project, []).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init(Project) ->
	.
