REBAR = $(shell pwd)/rebar3

all: get-deps compile

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

run:
	erl -pa ebin/ deps/*/ebin/ -eval "application:start(jsone)" -eval "application:start(erlexec)" -eval "application:start(ertl_433)"


