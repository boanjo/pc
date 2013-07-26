-module(file_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, code_change/3, handle_call/2, handle_info/2]).

init(File) ->
    {ok, Fd} = file:open(File, write),
    {ok, Fd}.

handle_event(Msg, Fd) ->
    Now = calendar:local_time(),
    io:format(Fd, "~s ~s ~p~n", [pc_utils:get_date_str(Now), pc_utils:get_time_str(Now), Msg]),
    {ok, Fd}.

terminate(_Args, Fd) ->
    file:close(Fd).

code_change(_, _, _) ->
    error_logger:error_msg("Unexpected code_change function call in ~p~n", [?MODULE]).

handle_call(_, _) ->
    error_logger:error_msg("Unexpected handle_call function call in ~p~n", [?MODULE]).

handle_info(_, _) ->
    error_logger:error_msg("Unexpected handle_info function call in ~p~n", [?MODULE]).
