-module(serial_terminal_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, code_change/3, handle_call/2, handle_info/2]).

init(_Args) ->
    
    {ok, []}.

handle_event(Msg, State) ->
    
    io:format("~s", [Msg]),
    {ok, State}.

terminate(_Args, _State) ->
    
    ok.

code_change(_, _, _) ->
    
    error_logger:error_msg("Unexpected code_change function call in ~p~n", [?MODULE]).

handle_call(_, _) ->
    
    error_logger:error_msg("Unexpected handle_call function call in ~p~n", [?MODULE]).

handle_info(_, _) ->
    
    error_logger:error_msg("Unexpected handle_info function call in ~p~n", [?MODULE]).
