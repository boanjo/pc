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


-define(TIMEOUT, 1*60*1000).

start() ->
    crypto:start(),
    emysql:start(),
    emysql:add_pool(pond_pool, 1,
		    "pc_epkboan_net", "Marigt123", "remote-mysql3.servage.net", 3306,
		    "pc_epkboan_net", utf8),
    Pid = spawn(?MODULE, init, []),
    register(mysql, Pid),
    Pid.

select() ->
    Result = emysql:execute(mysql_pool,
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
	    Curr#state{rain=list_to_float(trim_ws(Value))};
	"Flow" ->
	    Curr#state{flow=list_to_float(trim_ws(Value))};
	"Temp" ->
	    L = Curr#state.temp,
	    Curr#state{temp=[list_to_float(trim_ws(Value))|L]};
	"Level" ->
	    L = Curr#state.level,
	    Curr#state{level=[list_to_float(trim_ws(Value))|L]};
	"Water" ->
	    water_state(Curr, list_to_atom(trim_ws(Value)));
	_ ->
	    Curr
		
    end.
    
loop(Curr) ->
    receive
	{serial_msg, Str} ->
	    Parts = re:split(Str, "=", [{return,list}]),
	    io:format("Str = ~p~n", [Str]),
	    case length(Parts) of
		2 ->
		    [P1, P2] = Parts,
		    NewCurr = handle_string(Curr, P1, P2),
		    loop(NewCurr);
		_ ->
		    loop(Curr)
	    end;
	
	timer ->
	    timer:send_after(?TIMEOUT, timer),
	    Upd = water_time_at_tmo(Curr),
	    io:format("rain ~p~n", [Upd]),
	    Rain = Upd#state.rain - Upd#state.old_rain,
	    Flow = Upd#state.flow - Upd#state.old_flow,
	    WaterTime = Upd#state.water_on_time,
	    Temp = get_middle(Upd#state.temp),
	    Level = get_middle(Upd#state.level),

	    Now = calendar:local_time(),
	    Date = get_date_str(Now),
	    Time = get_time_str(Now),
	    
	    emysql:prepare(my_insert,
			   <<"INSERT INTO pond_event SET date=?, time=?, rain=?, flow=?, temp=?, level=?, water_time=?">>),
	    	    
	    Result = emysql:execute(pond_pool, my_insert, [Date, Time, Rain, Flow, Temp, Level, WaterTime]),

%%	    io:format("Printout ~p~n", [Result]),
	    
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

get_time_str({{_,_,_},{H, M, S}}) ->
    io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).

get_date_str({{Y, M, D}, {_,_,_}}) ->
    io_lib:format('~4..0b:~2..0b:~2..0b', [Y, M, D]).
    
