all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

skip-deps: get-deps compile
	./rebar skip_deps=true

run:
	erl -pa ebin/ deps/*/ebin/ -config etc/pc.config -config etc/app.config -config -config etc/simple_bridge.config -eval "application:start(sasl)" -eval "application:start(pc)" -eval "nitrogen_sup:start_link()" -sname boan -setcookie hej -detached

dev:
	erl -pa ebin/ deps/*/ebin/ -config etc/pc.config -config etc/app.config -config etc/sync.config -config etc/simple_bridge.config -eval "application:start(sync)" -eval "application:start(sasl)" -eval "application:start(pc)" -eval "nitrogen_sup:start_link()" -sname boan -setcookie hej

attach:
	erl -sname attach -setcookie hej -remsh boan@raspberrypi

plugins:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH;escript do-plugins.escript)


copy-static:
	@(cp -r deps/nitrogen_core/www/* priv/static//nitrogen/)


