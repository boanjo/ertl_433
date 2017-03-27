all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

run:
	erl -pa ebin/ deps/*/ebin/ -eval "application:start(jsone)" -eval "application:start(erlexec)" -eval "application:start(ertl_433)"


