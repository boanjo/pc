-module(pc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    pc_sup:start_link(),
    mysql_sup:start_link().


stop(_State) ->
    ok.
