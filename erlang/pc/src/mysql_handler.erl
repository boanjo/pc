-module(mysql_handler).

-export([start/0, select/0, stop/0, init/0, loop/1, handle_string/3, water_state/2]).


-record(state, 
	{
	  rain=0, 
	  flow=0, 
	  old_rain=0, 
	  old_flow=0, 
	  temp=[], 
	  level=[],
	  prev_water_state=off,
	  prev_water_on_timestamp,
	  water_on_time=0
	}). 


-define(TIMEOUT, 15*60*1000).

start() ->
    crypto:start(),
    emysql:start(),
    emysql:add_pool(pond_pool, 1,
		    "pc_epkboan_net", "Marigt123", "remote-mysql3.servage.net", 3306,
		    "pc_epkboan_net", utf8),
    Pid = spawn(?MODULE, init, []),
    register(mysql_handler, Pid),
    Pid.

select() ->
    Result = emysql:execute(pond_pool,
			    <<"select * from pc_epkboan_net.pond_event order by id desc limit 1">>),
    
    io:format("~n~p~n", [Result]).
    


stop() ->
    emysql:stop().    

init() ->
    
    Data = #state{
      rain=0,
      flow=0,
      old_rain=0,
      old_flow=0,
      temp=[],
      level=[],
      prev_water_state=off,
      prev_water_on_timestamp=calendar:local_time(),
      water_on_time=0
     },
    

    gen_event:start({local, confirmation_monitor}),
    gen_event:add_handler(confirmation_monitor, file_logger, ["/home/pi/conf.txt"]),

    gen_event:start({local, mysql_monitor}),
    gen_event:add_handler(mysql_monitor, file_logger, ["/home/pi/mysql.txt"]),

    ets:new(current_pond_status, [set, named_table]),

    timer:send_after(?TIMEOUT, timer),
    loop(Data),
    ok.   


trim_ws(Input) -> 
    re:replace(Input, "\\s+", "", [global, {return, list}]).


water_state(Curr, WaterState) 
  when Curr#state.prev_water_state =:= off, 
       WaterState =:= on ->
    NewState = Curr#state{prev_water_state=on,
			  prev_water_on_timestamp=calendar:local_time()},
    NewState;

water_state(Curr, WaterState) 
  when Curr#state.prev_water_state =:= on,
       WaterState =:= off ->
    Diff = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 
	calendar:datetime_to_gregorian_seconds(Curr#state.prev_water_on_timestamp),
    
    Acc = Diff + Curr#state.water_on_time,
    
    NewState = Curr#state{prev_water_state=off,
			  water_on_time=Acc},
    NewState;

water_state(Curr, _WaterState) ->
    Curr.
	  
water_time_at_tmo(Curr) 
  when Curr#state.prev_water_state =:= on ->
    Diff = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 
	calendar:datetime_to_gregorian_seconds(Curr#state.prev_water_on_timestamp),
    
    Acc = Diff + Curr#state.water_on_time,
    
    NewState = Curr#state{prev_water_on_timestamp=calendar:local_time(),
			  water_on_time=Acc},
    NewState;

water_time_at_tmo(Curr) ->
    Curr.

handle_string(Curr, Key, Value) ->
    case Key of
	"Rain" ->
	    Rain = list_to_float(trim_ws(Value)),
	    ets:insert(current_pond_status, {rain, Rain}),
	    ets:insert(current_pond_status, 
		       {time, pc_utils:get_time_str(calendar:local_time())}), 
	    Curr#state{rain=Rain};
	"Flow" ->
	    Flow = list_to_float(trim_ws(Value)),
	    ets:insert(current_pond_status, {flow, Flow}),
	    Curr#state{flow=Flow};
	"Temp" ->
	    L = Curr#state.temp,
	    Temp = list_to_float(trim_ws(Value)),
	    ets:insert(current_pond_status, {temp, Temp}),
	    Curr#state{temp=[Temp|L]};
	"Level" ->
	    L = Curr#state.level,
	    Level = list_to_float(trim_ws(Value)),
	    ets:insert(current_pond_status, {level, Level}),
	    Curr#state{level=[Level|L]};
	"Water" ->
	    State = list_to_existing_atom(trim_ws(Value)),
	    ets:insert(current_pond_status, {state, State}),
	    water_state(Curr, State);
	_ ->
	    Curr
		
    end.
    
loop(Curr) ->
    receive
	{serial_msg, Str} ->
	    Parts = re:split(Str, "=", [{return,list}]),
	   %% io:format("Str = ~p~n", [Str]),
	    case length(Parts) of
		2 ->
		    [P1, P2] = Parts,
		    NewCurr = handle_string(Curr, P1, P2),
		    loop(NewCurr);
		3 -> 
		    gen_event:notify(confirmation_monitor, Str),
		    loop(Curr);
		_ ->
		    loop(Curr)
	    end;
	
	timer ->
	    timer:send_after(?TIMEOUT, timer),
	    Upd = water_time_at_tmo(Curr),
	    gen_event:notify(mysql_monitor, Upd),
	    %%io:format("tmo ~p~n", [Upd]),
	    Rain = Upd#state.rain - Upd#state.old_rain,
%%	    io:format("1.", []),
	    Flow = Upd#state.flow - Upd#state.old_flow,
%%	    io:format("2.", []),
	    WaterTime = Upd#state.water_on_time,
%%	    io:format("3.", []),
	    Temp = get_middle(Upd#state.temp),
%%	    io:format("4.", []),
	    Level = get_middle(Upd#state.level),
%%	    io:format("5.", []),

	    Now = calendar:local_time(),
%%	    io:format("6.", []),
	    Date = pc_utils:get_date_str(Now),
%%	    io:format("7.", []),
	    Time = pc_utils:get_time_str(Now),
%%	    io:format("8. ~p~p~p~p~p~p~p~n", [Date, Time, Rain, Flow, Temp, Level, WaterTime]),
	    
	    emysql:prepare(my_insert,
			   <<"INSERT INTO pond_event SET date=?, time=?, rain=?, flow=?, temp=?, level=?, water_time=?">>),
	    	    
	    Result = emysql:execute(pond_pool, my_insert, [Date, Time, Rain, Flow, Temp, Level, WaterTime]),

	    %%io:format("Printout ~p~n", [Result]),
	    gen_event:notify(mysql_monitor, Result),
	    
	    
	    NewRain = Upd#state.rain,
	    NewFlow = Upd#state.flow,

	    NewCurr = Upd#state{rain=0,
				 flow=0,
				 old_rain=NewRain,
				 old_flow=NewFlow,
				 temp=[],
				 level=[],
				 water_on_time=0
				},
	    loop(NewCurr);

	Msg ->
	    error_logger:warning_msg("Unknown Message recd = ~p in ~p~n", [Msg, ?MODULE]),
	    loop(Curr)
    end.


get_middle(List) ->
    SortedList = lists:sort(List),
    Len = length(SortedList),
    Item = lists:nth(Len div 2, SortedList),
    Item.

    
