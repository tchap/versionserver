-module(versionserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, Val} = application:get_env(db_dir),
	Dirname = Val ++ "/",
	case filelib:ensure_dir(Dirname) of
		ok ->
			versionserver_sup:start_link();
		Error ->
			Error
	end.

stop(_State) ->
	ok.
