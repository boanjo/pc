-module(pc_server).
-behaviour(gen_server).

-include("../include/pc.hrl").

-export([start_link/0, send_to_serial/1]).
-export([log_terminal/1, log_file/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-export([get_all_devices/0,get_all_sensors/0]).
-export([device/2]).
-export([get_device/1, get_sensor/1, get_sensor_value/1]).
-record(state, {serial, acc_str, wd_recd}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    process_flag(trap_exit, true),

    gen_event:start({local, serial_monitor}),

    log_terminal(on),
    log_file(on, "log/serial.txt"),
    
    ets:new(sensor_table, [named_table, set, {keypos, #sensor.id}, public]),
    ets:new(device_table, [named_table, set, {keypos, #device.id}, public]),

    Self = self(),

    error_logger:info_msg("Startig ~p pid ~p~n ", [?MODULE, Self]),

    Pid = serial:start([{speed,9600},{open,"/dev/ttyACM0"}]),

    {ok, #state{serial = Pid, acc_str = [], wd_recd=true}}.


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
    %% [[Address, Unit]] = ets:match(device_table, {device, Id, '$1', '$2', '_', '_'}),
    %% send_to_serial("{device," 
    %% 		   ++ atom_to_list(Action) ++ "," 
    %% 		   ++ integer_to_list(Address) ++ ","
    %% 		   ++ integer_to_list(Unit) ++ "}").
ok.
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
    gen_server:call(?MODULE, {send, list_to_binary(Msg)}).

get_id([]) ->
    undefined;
get_id([[Id]]) ->
    Id.
	
check_serial(true, Pid) ->
    Pid;

check_serial(false, Pid) ->
    exit(Pid, kill),
    NewPid = serial:start([{speed,9600},{open,"/dev/ttyACM0"}]),
    NewPid.
		      
%% callbacks

handle_call({send, Binary}, _From, #state{serial = Pid} = State) ->
    %% error_logger:info_msg("Send ~p! ~n", [binary_to_list(Binary)]),
    Pid ! {send, Binary},
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    error_logger:info_msg("handle_call~n", []),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    error_logger:info_msg("handle_cast~n", []),
    {noreply, State}.

handle_info({data, Data}, State) ->
    %% error_logger:info_msg("Data=~p~n", [Data]),
    {NewAcc} = 
	handle_acc(State#state.acc_str ++ binary_to_list(Data)),	    
    {noreply, State#state{acc_str=NewAcc}};    


handle_info(Info, State) ->
    error_logger:info_msg("handle_info ~p~n", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



handle_acc(Acc) ->
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
		details(B),
		handle_acc(NewAcc)
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


update_sensor(Id, Value)->
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
    
    ok.
    

details({flow, Value, _Pulses}) ->
    update_sensor(200, Value),
    ok;

details({temp, Value}) ->
    update_sensor(201, Value),
    ok;

details({ph, Value}) ->
    update_sensor(202, Value),
    ok;

details({level, Value}) ->
    update_sensor(203, Value),
    ok;

details({cmd_time, _Value}) ->
    ok;


details({water, _Mode, _Value}) ->
    %% MatchList = ets:match(device_table, {device, '$1', Address, '$2', '_', '_'}),
    
    %% NewState = #device{address=Address, 
    %% 		       state=Action,
    %% 		       last_state_change_time=erlang:now()},
    %% update_device_state(MatchList, NewState),
    ok;

details(_) ->
    ok.

terminate(Reason, _State) ->
    error_logger:error_msg("~p ~p: ~p~n ", [erlang:localtime(), ?MODULE, Reason]),    
    ok.
    
