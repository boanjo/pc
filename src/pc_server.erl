-module(pc_server).
-behaviour(gen_server).

-include("../include/pc.hrl").

-export([start_link/0, send_to_serial/1]).
-export([log_terminal/1, log_file/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-export([get_all_devices/0,get_all_sensors/0]).
-export([device/2, add_listener/1]).
-export([get_device/1, get_sensor/1, get_sensor_value/1]).

-record(state, {serial, acc_str, wd_recd, listeners}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    process_flag(trap_exit, true),

    gen_event:start({local, serial_monitor}),


    case application:get_env(pc, device) of
	{ok, List} ->
	    check_devices_for_timers(List);
	_ -> 
	    ok
    end,

    log_terminal(off),
    log_file(on, "log/serial.txt"),
    
    ets:new(sensor_table, [named_table, set, {keypos, #sensor.id}, public]),
    ets:new(device_table, [named_table, set, {keypos, #device.id}, public]),

    ets:insert(device_table, #device{id=auto_level, state=on,last_state_change_time=erlang:now()}),
    ets:insert(device_table, #device{id=water, state=off,last_state_change_time=erlang:now()}),
    ets:insert(device_table, #device{id=sprinkler1, state=off,last_state_change_time=erlang:now()}),
    ets:insert(device_table, #device{id=sprinkler2, state=off,last_state_change_time=erlang:now()}),

    Self = self(),

    error_logger:info_msg("Startig ~p pid ~p~n ", [?MODULE, Self]),

    Pid = serial:start([{speed,115200},{open,"/dev/ttyACM0"}]),

    {ok, #state{serial = Pid, acc_str = [], wd_recd=true, listeners=[]}}.

check_devices_for_timers([]) ->
    ok;
check_devices_for_timers([Device|Devices]) ->
    {Id, _Name, List} = Device,
    start_timers(Id, List),
    check_devices_for_timers(Devices).

start_timers(_Id, []) ->
    ok;

start_timers(Id, [{Cmd, Time}|Events]) ->    
    Msg = {Id, Cmd, Time, permanent},
    gen_server:cast(?MODULE, {start_timer, Msg}),
    start_timers(Id, Events).


date_diff(D1, D2) ->    
    calendar:datetime_to_gregorian_seconds(D1) -
        calendar:datetime_to_gregorian_seconds(D2).

next_day({Date, Time}) ->
    Days = calendar:date_to_gregorian_days(Date),
    NextDate = calendar:gregorian_days_to_date(Days+1),
    {NextDate, Time}.


log_terminal(on) ->
    gen_event:add_handler(serial_monitor, serial_terminal_logger, []);
log_terminal(off) ->
    gen_event:delete_handler(serial_monitor, serial_terminal_logger, []).

log_file(on, File) ->
    gen_event:add_handler(serial_monitor, serial_file_logger, [File]);
log_file(off, _File) ->
    gen_event:delete_handler(serial_monitor, serial_file_logger, []).

get_next(_Table, '$end_of_table', Acc) ->
    Acc;
get_next(Table, Prev, Acc) ->
    [Val] = ets:lookup(Table, Prev),
    get_next(Table, ets:next(Table, Prev), [Val|Acc]).

get_all_devices() ->
    Table = device_table,
    First = ets:first(Table),
    get_next(Table, First, []).

get_all_sensors() ->
    Table = sensor_table,
    First = ets:first(Table),
    get_next(Table, First, []).

lookup(Table, Id) ->
    Val = ets:lookup(Table, Id),
    case Val of
	[] -> not_found;
	List -> [Ret] = List, 
		Ret
    end.


device(Id, Action) ->
    send_to_serial("{" ++ atom_to_list(Id) ++ "," 
    		   ++ atom_to_list(Action) ++ "}").

get_sensor(Id) ->
    lookup(sensor_table, Id).

get_sensor_value(Id) ->
    Rec = lookup(sensor_table, Id),
    case Rec of
	{sensor, Id, Val, _Min, _Max, _Today, _Upd} ->
	    Val;
	_ -> not_found
    end.

get_device(Id) ->
    lookup(device_table, Id).

send_to_serial(Msg) ->
    gen_server:cast(?MODULE, {send, list_to_binary(Msg)}).

add_listener(Pid) ->
    gen_server:call(?MODULE, {listener, Pid}).
	
%% callbacks

handle_call({listener, Pid}, _From, State) ->
    %% error_logger:info_msg("Send ~p! ~n", [binary_to_list(Binary)]),

    erlang:monitor(process, Pid),

    NewAcc = lists:append(State#state.listeners, [Pid]),
    {reply, ok, State#state{listeners=NewAcc}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    error_logger:info_msg("handle_call~n", []),
    {reply, Reply, State}.

handle_cast({send, Binary}, #state{serial = Pid} = State) ->
    %% error_logger:info_msg("Send ~p! ~n", [binary_to_list(Binary)]),
    Pid ! {send, Binary},
    {noreply, State};

handle_cast({start_timer, Msg}, State) -> 
        {_, _, EventTime, _} = Msg,
    Now = calendar:local_time(),
    {NowDate, _} = Now,
    TimeOut = {NowDate, EventTime},
    Diff = date_diff(TimeOut, Now),
    
    %% We use 1 instead of 0 to make sure we don't start new timer 
    %% for todays timeout
    Secs = if Diff > 1  -> Diff;
	      Diff =< 1 -> date_diff(next_day(TimeOut), Now)
	   end,
    
    timer:send_after(Secs*1000, self(), {timeout, Msg}),
    error_logger:info_msg("Starting timer with tmo value ~p secs~n", [Secs]),
    {noreply, State};

handle_cast(_Msg, State) ->
    error_logger:info_msg("handle_cast~n", []),
    {noreply, State}.

handle_info({timeout, Msg}, State) ->    
    error_logger:info_msg("Timeout Msg=~p Time=~p ~n", [Msg, calendar:local_time()]),
    {Id, Cmd, _Time, Type} = Msg,
    device(Id, Cmd),

    case Type of
	permanent ->
	    gen_server:cast(?MODULE, {start_timer, Msg});
	_Transient -> ok
    end,
    {noreply, State};

handle_info({data, Data}, State) ->
    {NewList} = 
	handle_acc(State#state.acc_str ++ binary_to_list(Data), State#state.listeners),	    
    {noreply, State#state{acc_str=NewList}};    

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->

    io:format("PID down! ~p ~n", [Pid]),
    NewList =  lists:delete(Pid, State#state.listeners),

    {noreply, State#state{listeners=NewList}};    


handle_info(Info, State) ->
    error_logger:info_msg("handle_info ~p~n", [Info]),
    {noreply, State}.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



handle_acc(Acc, Listeners) ->
    case string:str(Acc, "\r\n") of
	0 -> 
	    {Acc};
	Index ->
	    Line = string:substr(Acc, 1, Index-1),
	    NewAcc = string:substr(Acc, Index+2),
	    %%error_logger:info_msg("~p ~p: ~p~n ", [erlang:localtime(), ?MODULE, Line]),
	    gen_event:notify(serial_monitor, io_lib:format("~p ~p: ~p~n ", 
							 [erlang:localtime(), 
							  ?MODULE, Line])),
	    try 
		{ok, Tokens, _} = erl_scan:string(Line),
		{ok, B} = erl_parse:parse_term(Tokens), 
		details(B, Listeners),
		handle_acc(NewAcc, Listeners)
	    catch
		_:_ ->
		    error_logger:error_msg("Incorrect erlang term recd, skipping! ~n", []),
		    {[]}
	    end
    end.


get_max(not_found, Value, _Date) ->
    Value;
get_max({sensor, _Id, _Val, _Prev, _Min, Max, Today, _Upd}, Value, Date) 
  when Max > Value,  Date == Today ->
    Max;
get_max(_, Value, _Date) ->
    Value.

get_min(not_found, Value, _Date) ->
    Value;
get_min({sensor, _Id, _Val, _Prev, Min, _Max, Today, _Upd}, Value, Date) 
  when Min < Value,  Date == Today ->
    Min;
get_min(_, Value, _Date) ->
    Value.


validate_sensor_value(not_found, Value) ->
    Value;
validate_sensor_value({sensor, _Id, _Val, Prev, _Min, _Max, _Today, _Upd}, Value) 
  when (Prev + 1.0) > Value, (Prev - 1.0) < Value ->
    Value;
validate_sensor_value({sensor, _Id, Val, _Prev, _Min, _Max, _Today, _Upd}, _Value) ->
    Val.


update_sensor(Id, Value, Listeners)->
    Sensor = lookup(sensor_table, Id),

    Date = erlang:date(), 

    ValidatedValue = validate_sensor_value(Sensor, Value),

    Max = get_max(Sensor, ValidatedValue, Date),
    Min = get_min(Sensor, ValidatedValue, Date),
    
    ets:insert(sensor_table, 
	       #sensor{id=Id,
		       value=ValidatedValue,
		       prev_value=Value,
		       min=Min,
		       max=Max,
		       today=Date,
		       last_update_time=erlang:now()}),

    
    notify_listeners(Listeners, Id, ValidatedValue),
    ok.
update_device(Id, State, _Listeners)->
    ets:insert(device_table, 
	       #device{id=Id,
		       state=State,
		       last_state_change_time=erlang:now()}),

    
%%    notify_listeners(Listeners, Id, ValidatedValue),
    ok.
    
notify_listeners([], _Id, _Value) ->
    ok;
notify_listeners([Pid|Tail],Id, Value) ->
    Pid ! {Id, Value},

    notify_listeners(Tail, Id, Value).

details({flow, Value, _Pulses}, L) ->
    update_sensor(flow, Value, L),
    ok;

details({temp, Value}, L) ->
    update_sensor(temp, Value, L),
    ok;

details({ph, Value}, L) ->
    update_sensor(ph, Value, L),
    ok;

details({level, Value}, L) ->
    update_sensor(level, Value, L),
    ok;

details({cmd_time, _Value}, _L) ->
    ok;


details({water, State}, L) ->
    update_device(water, State, L),  
    ok;
details({auto_level, State}, L) ->
    update_device(auto_level, State, L),  
    ok;
details({sprinkler1, State}, L) ->
    update_device(sprinkler1, State, L),  
    ok;
details({sprinkler2, State}, L) ->
    update_device(sprinkler2, State, L),  
    ok;

details(_, _L) ->
    ok.

terminate(Reason, _State) ->
    error_logger:error_msg("~p ~p: ~p~n ", [erlang:localtime(), ?MODULE, Reason]),    
    ok.
    
