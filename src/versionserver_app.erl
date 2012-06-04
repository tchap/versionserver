-module(versionserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Pid = versionserver_sup:start_link(),
    {ok, Pid}.

stop(_State) ->
    ok.
