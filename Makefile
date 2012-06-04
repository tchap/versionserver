DBDIR=/tmp/versionserver_db

.PHONY: install run

all:
	./rebar compile

install:
	./scripts/create_tables.escript '$(DBDIR)'
