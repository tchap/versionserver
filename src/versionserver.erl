-module(versionserver).

-behaviour(gen_server).

-define(PROJ_MODULE, versionserver_proj).

%% API functions
-export([start/0, start_link/0, stop/0,
	 get_build_number/3, set_build_number/3, delete_project/2]).

%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_build_number(Proj, Version={Maj, Min, Rel})
    when is_atom(Proj), lists:all(fun erlang:is_integer/1, 
	    			  [Maj, Min, Rel]) ->
	gen_server:call(?MODULE, {get_build_number, Proj, Version}).

set_build_number(Proj, Version={Maj, Min, Rel}, Build) ->
    when is_atom(Proj), lists:all(fun erlang:is_integer/1, 
	    			  [Maj, Min, Rel, Build]) ->
	gen_server:cast(?MODULE, {set_build_number, Proj, Version, Build}).

delete_project(Proj) when is_atom(Proj) ->
	gen_server:cast(?MODULE, {delete_project, Proj}).

stop() ->
	gen_server:call(?MODULE, stop).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
	process_flag(trap_exit, true),
	ets:new(?MODULE, [set, named_table])},
	{ok, none}.

handle_call({get_build_number, Proj, Version}, From, State) ->
	Request = {get_build_number, Version, From},
	ProjPid = case ets:lookup(?MODULE, Proj) of
		[{Proj, Pid}] ->
			Pid;
		[] ->
			Pid = gen_server:start_link(?PROJ_MODULE, [Proj], []),
			ets:insert(?MODULE, {Proj, Pid}),
			Pid
	end,
	gen_server:cast(Pid, Request),
	{noreply, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

handle_cast(Msg={set_build_number, _Version, _Build}, State) ->
	{noreply, State};
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

handle_info({'EXIT', Pid, _Reason}, State) ->
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Private functions
%% ===================================================================
