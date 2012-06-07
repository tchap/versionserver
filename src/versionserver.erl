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

-module(versionserver).
-author("Ondrej Kupka <ondra.cap@gmail.com>").

-behaviour(gen_server).

-define(INT(Var), is_integer(Var)).

%% API
-export([start/0, start_link/0, stop/0,
	 next_build_number/2, set_build_number/3,
	 clean_project/1, delete_project/1]).

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

next_build_number(Proj, Version={Maj, Min, Rel})
    when is_atom(Proj), ?INT(Maj), ?INT(Min), ?INT(Rel) ->
	gen_server:call(?MODULE, {next_build_number, Proj, Version}).

set_build_number(Proj, Version={Maj, Min, Rel}, Build)
    when is_atom(Proj), ?INT(Maj), ?INT(Min), ?INT(Rel), ?INT(Build) ->
	gen_server:cast(?MODULE, {set_build_number, Proj, Version, Build}).

clean_project(Proj) when is_atom(Proj) ->
	gen_server:cast(?MODULE, {clean_project, Proj}).

delete_project(Proj) when is_atom(Proj) ->
	gen_server:cast(?MODULE, {delete_project, Proj}).

stop() ->
	gen_server:call(?MODULE, stop).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
	process_flag(trap_exit, true),
	ets:new(?MODULE, [set, named_table]),
	{ok, empty_state}.

handle_call({next_build_number, Proj, Version}, From, State) ->
	Pid = get_proj_pid(Proj),
	versionserver_proj:reply_build_number(Pid, Version, From),
	{noreply, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

handle_cast({set_build_number, Proj, Version, Build}, State) ->
	Pid = get_proj_pid(Proj),
	versionserver_proj:set_build_number(Pid, Version, Build),
	{noreply, State};
handle_cast({clean_project, Proj}, State) ->
	Pid = get_proj_pid(Proj),
	versionserver_proj:clean_project(Pid),
	{noreply, State};
handle_cast({delete_project, Proj}, State) ->
	Pid = get_proj_pid(Proj),
	versionserver_proj:delete_project(Pid),
	{noreply, State}.

handle_info({'EXIT', ProjPid, _Reason}, State) ->
	cleanup(ProjPid),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ets:delete(?MODULE),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Private functions
%% ===================================================================

get_proj_pid(Proj) ->
	case ets:lookup(?MODULE, Proj) of
		[{Proj, Pid}] ->
			Pid;
		[] ->
			{ok, Pid} = versionserver_proj:start_link(Proj),
			ets:insert(?MODULE, {Proj, Pid}),
			Pid
	end.

cleanup(ProjPid) ->
	ets:match_delete(?MODULE, {'_', ProjPid}).
