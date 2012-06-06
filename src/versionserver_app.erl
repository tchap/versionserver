-module(versionserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Dirname = application:get_env(db_dir),
	case filelib:ensure_dir(Dirname) of
		ok -> 
			Pid = versionserver_sup:start_link(),
			{ok, Pid};
		Error ->
			Error
	end.

stop(_State) ->
	ok.
