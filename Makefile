all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

skip-deps: get-deps compile
	./rebar skip_deps=true

run:
	erl -pa ebin/ -eval "application:start(pc)" -sname boan -setcookie hej -detached

attach:
	erl -sname attach -setcookie hej -remsh boan@raspberrypi



plugins:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH;escript do-plugins.escript)



