-module(feeder).
-export([run/1, start_timers/1, loop/0]).

run(Events) when 
      is_list(Events) ->
    spawn(?MODULE, start_timers, [Events]),
    ok.
    
start_timer([]) ->
    ok;

start_timer([EventTime|Events]) ->
    Now = calendar:local_time(),
    {NowDate, _} = Now,
    TimeOut = {NowDate, EventTime},
    Diff = date_diff(TimeOut, Now),
    
    Secs = if Diff > 0  -> Diff;
              Diff =< 0 -> date_diff(next_day(TimeOut), Now)
           end,
    
    timer:send_after(Secs*1000, feed),
    io:format("Starting timer with tmo value ~p secs~n", [Secs]),
    start_timer(Events).


start_timers(Events) ->
    start_timer(Events),
    loop().

date_diff(D1, D2) ->
    calendar:datetime_to_gregorian_seconds(D1) -
	calendar:datetime_to_gregorian_seconds(D2).

next_day({Date, Time}) ->
    Days = calendar:date_to_gregorian_days(Date),
    NextDate = calendar:gregorian_days_to_date(Days+1),
    {NextDate, Time}.

loop() ->
    receive
	feed ->
	    serial:send("feed=40"),
	    io:format("Feeding!~n", []),
	    loop();
	_ ->
	    error_logger:warning_msg("Unrecognized msg received in ~p~n", [?MODULE]),
	    loop()
    end.

