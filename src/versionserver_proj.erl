-module(versionserver_proj).

-behaviour(gen_server).

%% API functions
-export([start/1, start_link/1, stop/1,
	 reply_build_number/3, set_build_number/3, delete_project/1]).

%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {table}).

%% ===================================================================
%% API functions
%% ===================================================================

start(Project) when is_atom(Project) ->
	get_server:start(?MODULE, [Project], []).

start_link(Project) when is_atom(Project) ->
	gen_server:start_link(?MODULE, [Project], []).

reply_build_number(ProjPid, Version, To) ->
	gen_server:cast(ProjPid, {reply_build_number, Version, To}).

set_build_number(ProjPid, Version, Build) ->
	gen_server:cast(ProjPid, {set_build_number, Version, Build}).

delete_project(ProjPid) ->
	gen_server:cast(ProjPid, delete_project).

stop(ProjPid) ->
	get_server:call(ProjPid, stop).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init(Project) ->
	try
		DbPath = get_dbfile_path(Project),
		case dets:open_file(Project, [{auto_save, 0},
					      {file, DbPath}]) of
			{ok, Name} ->
				{ok, #state{table=Name}};
			{error, Reason} ->
				throw(Reason)
		end
	of
		{ok, State} -> {ok, State}
	catch throw:Throw ->
		{stop, Throw}
	end.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

handle_cast({reply_build_number, Version, To}, State) ->
	case next_buildnum(State#state.table, Version) of
		Build when is_integer(Build) ->
			gen_server:reply(To, Build),
			{noreply, State};
		{error, Reason} ->
			gen_server:reply(To, {error, Reason}),
			{stop, Reason, State};
		Otherwise ->
			gen_server:reply(To, {error, Otherwise}),
			{stop, Otherwise, State}
	end;
handle_cast({set_build_number, Version, Build}, State) ->
	set_buildnum(State#state.table, Version, Build),
	{noreply, State};
handle_cast(delete_project, State) ->
	del_project(State#state.table),
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	dets:close(State#state.table),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Private functions
%% ===================================================================

get_dbfile_path(Proj) ->
	Dirname = application:get_env(db_dir),
	Filename = lists:concat([Proj, ".dets"]),
	Path = file:join(Dirname, Filename),
	case filelib:is_file(Path) of
		true -> Path;
		false ->
			case filelib:ensure_dir(Dirname) of
				ok -> Path;
				{error, Reason} -> throw(Reason)
			end
	end.

next_buildnum(Table, Version) ->
	dets:update_counter(Table, Version, 1).

set_buildnum(Table, Version, Build) ->
	case dets:insert(Table, {Version, Build}) of
		ok -> ok;
		{error, Reason} -> erlang:exit(Reason)
	end.

del_project(Table) ->
	case dets:delete_all_objects(Table) of
		ok -> ok;
		{error, Reason} -> erlang:exit(Reason)
	end.
