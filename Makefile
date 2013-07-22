all:
	@rebar compile skip_deps=true

compile:
	@rebar compile

deps:
	@rebar get-deps
	@rebar compile

clean:
	@rebar clean skip_deps=true
	
clean_all:
	@rebar clean

eunit:
	@rebar eunit skip_deps=true

.PHONY: deps compile all test eunit clean clean_all
