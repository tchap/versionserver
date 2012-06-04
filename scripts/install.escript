#!/usr/bin/env escript
%%! -pz ../versionserver

-include_lib("versionserver/include/versionserver_proj.hrl").

-define(TABLE_NAME, versionserver_project).

main([Dirname]) ->
	case filelib:ensure_dir(Dirname) of
		ok ->
			install_database(Dirname);
		{error, Reason} ->
			io:format(standard_error, 
				  "I/O error occured: ~w~n", [Reason]),
			halt(1)
	end;

main(_) ->
	usage().

usage() ->
	io:format("Usage: install.escript <directory>"),
	halt(1).

install_database(Dirname) ->
	application:set_env(mnesia, dir, Dirname),
	case mnesia:create_schema([node()]) of
		ok ->
			create_tables();
		{error, Reason} ->
			io:format(standard_error,
				  "Failed to create Mnesia schema: ~w~n",
				  [Reason]),
			halt(1)
	end.

create_tables() ->
	application:start(mnesia),
	case mnesia:create_table(versionserver_proj,
				 [{attributes, 
				   record_info(fields, versionserver_proj)},
		   		  {index, [#versionserver_proj.version]},
			  	  {disc_copies, [node()]}]) of
		  {atomic, ok} ->
			  io:format("Mnesia tables created successfully.~n");
		  {aborted, Reason} ->
			  io:format(standard_error,
			  	    "Failed to create Mnesia tables: ~w~n",
			    	    [Reason]),
			  application:stop(mnesia),
			  halt(1)
	end,
	application:stop(mnesia).
