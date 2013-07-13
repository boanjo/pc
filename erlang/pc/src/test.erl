-module(test).
-export([start/0]).

start() ->
    pc_app:start(2,2).
