-module(pc_utils).
-export([get_time_str/1, get_date_str/1]).

get_time_str({{_,_,_},{H, M, S}}) ->
    io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).

get_date_str({{Y, M, D}, {_,_,_}}) ->
    io_lib:format('~4..0b-~2..0b-~2..0b', [Y, M, D]).
