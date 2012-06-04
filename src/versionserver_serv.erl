-module(versionserver_serv).

-behaviour(gen_server).

%% API functions
-export([start/2, start_link/2, get_build_number/3, stop/1]).

%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {projsup, projects=dict:new()}).

%% ===================================================================
%% API functions
%% ===================================================================

start(Name, ProjSup) when is_pid(ProjSup) ->
	gen_server:start({local, Name}, ?MODULE, ProjSup, []);
start(_Name, _ProjSup) ->
	{error, {badarg, "argument must be a pid"}}.

start_link(Name, ProjSup) when is_pid(ProjSup) ->
	gen_server:start_link({local, Name}, ?MODULE, ProjSup, []).
start(_Name, _ProjSup) ->
	{error, {badarg, "argument must be a pid"}}.

get_build_number(Name, Proj, Version={Maj, Min, Rel})
    when is_atom(Proj), is_integer(Maj), is_integer(Min), is_integer(Rel) ->
	gen_server:call(Name, {build_number, Proj, Version}).

stop(Name) ->
	gen_server:call(Name, stop).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init(ProjSup) ->
	{ok, #state{projsup=ProjSup}}.

handle_call({build_number, Proj, Version}, From, State) ->
	Projects = State#projects,
	case dict:find(Proj, Projects) of
		{ok, Id} ->
			gen_server:cast(Id, {build_number, Proj, Version, Id}),
			{noreply, State};
		error ->
			Id = supervisor:start_child(State#projsup, Proj),
			UpdatedProjects = dict:store(Proj, Id, Projects),
			gen_server:cast(Id, {build_number, Proj, Version, Id}),
			{noreply, State#state{projects=UpdatedProjects}}
	end;
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
