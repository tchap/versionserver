-module(versionserver_serv).

-behaviour(gen_server).

-define(PROJ_SUP, version_proj_sup).

%% API functions
-export([start/1, start_link/1, get_build_number/3, delete_project/2, stop/1]).

%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Server state record
-record(state, {projects=dict:new()}).

%% ===================================================================
%% API functions
%% ===================================================================

start(Name) ->
	gen_server:start({local, Name}, ?MODULE, [], []).

start_link(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [], []).

get_build_number(Name, Proj, Version={Maj, Min, Rel})
    when is_atom(Proj), is_integer(Maj), is_integer(Min), is_integer(Rel) ->
	gen_server:call(Name, {build_number, Proj, Version}).

delete_project(Name, Proj) when is_atom(Proj) ->
	gen_server:cast(Name, {delete_project, Proj}).

stop(Name) ->
	gen_server:call(Name, stop).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
	State = #state{},
	{ok, State}.

handle_call({build_number, Proj, Version}, From, State) ->
	Projects = State#state.projects,
	case dict:find(Proj, Projects) of
		{ok, Id} ->
			gen_server:cast(Id, {build_number, Proj, Version, From}),
			{noreply, State};
		error ->
			Id = supervisor:start_child(?PROJ_SUP, Proj),
			UpdatedProjects = dict:store(Proj, Id, Projects),
			gen_server:cast(Id, {build_number, Proj, Version, From}),
			{noreply, State#state{projects=UpdatedProjects}}
	end;
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

handle_cast(Msg={delete_project, Proj}, State) ->
	Projects = State#state.projects,
	case dict:find(Proj, Projects) of
		{ok, Id} ->
			gen_server:call(Id, Msg),
			UpdatedProjects = dict:erase(Id, Projects),
			{noreply, State#state{projects=UpdatedProjects}};
		error ->
			{noreply, State}
	end.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
