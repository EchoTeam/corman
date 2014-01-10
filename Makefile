.PHONY: all clean test-unit test-ct check

REBAR= `which ./rebar || rebar`

all:
		$(REBAR) get-deps
		$(REBAR) compile

test-unit:
		$(REBAR) eunit skip_deps=true

test-ct:
		$(REBAR) ct skip_deps=true

check:		test-unit test-ct

clean:
		$(REBAR) clean
		rm -rf ./ebin
		rm -rf ./deps
		rm -rf ./erl_crash.dump
