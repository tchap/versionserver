DBDIR=/tmp/versionserver_db

.PHONY: install

all:
	./rebar compile

install:
	./scripts/install.escript '$(DBDIR)'
