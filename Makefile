REBAR ?= rebar
ifneq ($(wildcard rebar),)
	REBAR := ./rebar
endif

.PHONY: clean test docs docsclean go quick dialyzer

all: get-deps compile

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile
	$(REBAR) skip_deps=true xref

quick:
	$(REBAR) skip_deps=true compile
	$(REBAR) skip_deps=true xref

clean:
	$(REBAR) clean
	rm -f erl_cache

test: compile
	$(REBAR) skip_deps=true eunit

docs: docsclean
	ln -s . doc/doc
	$(REBAR) skip_deps=true doc

docsclean:
	rm -f doc/*.html doc/*.css doc/erlang.png doc/edoc-info doc/doc

go:
	erl -sname bingo -pa deps/*/ebin -pa ebin/ -s reloader start -s bingo_app start ${EXTRA_ARGS}

dialyzer:
	dialyzer -c ebin/ -Wunmatched_returns -Werror_handling -Wrace_conditions


