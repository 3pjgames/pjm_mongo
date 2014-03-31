default: compile

clean:
	rebar clean

deps:
	rebar get-deps compile

compile:
	rebar compile skip_deps=true

test:
ifdef suites
	rebar eunit skip_deps=true suites=$(suites)
else
	rebar eunit skip_deps=true
endif

doc:
	rebar doc skip_deps=true

.PHONY: default clean deps compile test doc
