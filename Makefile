.PHONY: deps test

all: deps compile
	./rebar compile

compile: deps
	./rebar compile

deps:
	test -d deps || ./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

cleantest:
	rm -rf .eunit/*test*

DIALYZER_APPS = kernel stdlib erts sasl ssl crypto public_key

include tools.mk
