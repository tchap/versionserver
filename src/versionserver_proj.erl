-module(versionserver_proj).

-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {proj}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Project) when is_atom(Project) ->
	gen_server:start_link(?MODULE, [Project], []).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init(Project) ->
	{ok, #state{proj=Project}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({get_build_number, Version, Id}, State) ->
	case get_build_number(State#state.proj, Version) of
		Build when is_integer(Build) ->
			gen_server:reply(Id, Build),
			ok = set_build_number(State#state.proj,
					      Version, Build + 1),
			{noreply, State};
		{error, Reason} ->
			gen_server:reply(Id, {error, Reason}),
			{stop, Reason, State};
		_ ->
			Reason = unknown_error,
			gen_server:reply(Id, {error, Reason}),
			{stop, Reason, State}
	end;
handle_cast({delete_project, _Proj}, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Private functions
%% ===================================================================

get_build_number(Proj, {Maj, Min, Rel}) ->
	Version = list_to_atom(lists:concat([Maj, '.', Min, '.', Rel])),
	Pattern = {Proj, Version, '$1'},
	Fun = fun() -> 
		mnesia:dirty_match_object(versionserver_proj, Pattern) end,
	try mnesia:sync_dirty(Fun, []) of
		{Proj, Version, Build} when is_integer(Build) ->
			Build;
		_ -> {error, unknown_error}
	catch exit:Exit ->
		{error, Exit}
	end.

set_build_number(_Proj, {_Maj, _Min, _Rel}, _Build) ->
	ok.
