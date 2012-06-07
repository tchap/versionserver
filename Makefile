.PHONY: all compile clean

all: compile

compile:
	./rebar compile

test: compile

clean:
	./rebar clean
