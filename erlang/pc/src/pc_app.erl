-module(pc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _ApplicationArgs) ->
    pc_sup:start_link(),
    serial:start(),
    feeder:run([{18,30,0}, {7,30,0}]),
    mysql_handler:start(),
    ok.

stop(_State) ->
    ok.
