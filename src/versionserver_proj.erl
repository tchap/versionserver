-module(versionserver_proj).

-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% Server callbacks
-export([init/1]).

-record(state, {versions})

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

handle_call({get_build_number, 
	     Proj, Version={Maj, Min, Rel}, Id}, _From, State) ->
	.

handle_cast({delete_project, Proj}, State) ->
	.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, State) ->
	ok.

code_change(OldVsn, State, Extra) ->
	{ok, State}.
