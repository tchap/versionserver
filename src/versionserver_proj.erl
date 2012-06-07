%% Copyright (c) 2012, Ondrej Kupka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met: 
%%
%% 1. Redistributions of source code must retain the above copyright notice, this
%%    list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above copyright notice,
%%    this list of conditions and the following disclaimer in the documentation
%%    and/or other materials provided with the distribution. 
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(versionserver_proj).
-author("Ondrej Kupka <ondra.cap@gmail.com>").

-behaviour(gen_server).

%% API
-export([start/1, start_link/1, stop/1,
	 reply_build_number/3, set_build_number/3,
	 clean_project/1, delete_project/1]).

%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {table}).

%% ===================================================================
%% API functions
%% ===================================================================

start(Project) when is_atom(Project) ->
	gen_server:start(?MODULE, Project, []).

start_link(Project) when is_atom(Project) ->
	gen_server:start_link(?MODULE, Project, []).

reply_build_number(ProjPid, Version, To) ->
	gen_server:cast(ProjPid, {reply_build_number, Version, To}).

set_build_number(ProjPid, Version, Build) ->
	gen_server:cast(ProjPid, {set_build_number, Version, Build}).

clean_project(ProjPid) ->
	gen_server:cast(ProjPid, clean_project).

delete_project(ProjPid) ->
	gen_server:cast(ProjPid, delete_project).

stop(ProjPid) ->
	gen_server:call(ProjPid, stop).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init(Project) ->
	DbPath = get_dbfile_path(Project),
	case dets:open_file(Project, [{auto_save, 0},
				      {file, DbPath}]) of
		{ok, Name} ->
			{ok, #state{table=Name}};
		{error, Reason} ->
			{stop, Reason}
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
handle_cast(clean_project, State) ->
	cln_project(State#state.table),
	{noreply, State};
handle_cast(delete_project, State) ->
	del_project(State#state.table),
	{stop, normal, deleted}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, deleted) ->
	ok;
terminate(_Reason, State) ->
	dets:close(State#state.table),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Private functions
%% ===================================================================

get_dbfile_path(Proj) ->
	{ok, Dirname} = application:get_env(db_dir),
	Filename = atom_to_list(Proj) ++ ".dets",
	filename:join(Dirname, Filename).

next_buildnum(Table, Version) ->
	try 
		dets:update_counter(Table, Version, 1)
	catch
		error:_ ->
			case dets:insert(Table, {Version, 1}) of
				ok -> 1;
				Error -> Error
			end
	end.

set_buildnum(Table, Version, Build) ->
	case dets:insert(Table, {Version, Build}) of
		ok -> ok;
		{error, Reason} -> erlang:exit(Reason)
	end.

del_project(Table) ->
	ok = dets:close(Table),
	ok = file:delete(get_dbfile_path(Table)).

cln_project(Table) ->
	case dets:delete_all_objects(Table) of
		ok -> ok;
		{error, Reason} -> erlang:exit(Reason)
	end.
