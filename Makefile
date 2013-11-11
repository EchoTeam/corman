.PHONY: all test clean

REBAR="./rebar"

all:
		$(REBAR) get-deps
		$(REBAR) compile

test:
		$(REBAR) eunit skip_deps=true

clean:
		$(REBAR) clean
		rm -rf ./ebin
		rm -rf ./deps
		rm -rf ./erl_crash.dump
