-module(mysql_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, upload/1, select/0,
	 get_min_max_last_week/1, get_min_max_today/1]).

start_link() ->
    
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    crypto:start(),
    application:start(emysql),

    error_logger:info_msg("Starting up ~p~n", [?MODULE]),

    {ok, User} = application:get_env(pc, mysql_user), 
    {ok, Password} = application:get_env(pc, mysql_password), 
    {ok, Server} = application:get_env(pc, mysql_server), 
    {ok, Port} = application:get_env(pc, mysql_port),
    {ok, Database} = application:get_env(pc, mysql_database), 
    {ok, Table} = application:get_env(pc, mysql_table), 
    {ok, Sensors} = application:get_env(pc, mysql_upload_sensor_list), 
    
    
    emysql:add_pool(pc_pool, 1,
		    User, Password, Server, Port, Database,
		    utf8),    


    DbTable = Database ++ "." ++ Table, 
    
    SelectMinMaxLastWeek = "SELECT date, MAX(value), MIN(value) FROM " ++
	DbTable ++ " where sensor=? AND date BETWEEN " ++ 
	" DATE_SUB(NOW(), INTERVAL 7 DAY) AND NOW() GROUP BY DATE(date)",
    
    io:format("~p~n",[SelectMinMaxLastWeek]),

    emysql:prepare(select_min_max_last_week,
		   list_to_binary(SelectMinMaxLastWeek)),
    
    
    SelectMinMaxToday = "SELECT date, MAX(value), MIN(value) FROM " ++ 
	DbTable ++ " where sensor=? AND DATE(date) = DATE(NOW())",

    emysql:prepare(select_min_max_today,
		   list_to_binary(SelectMinMaxToday)),
    
    InsertSensor = "INSERT INTO " ++ DbTable ++ 
	" SET sensor=?, date=?, time=?, value=?",

    emysql:prepare(insert_sensor,
		   list_to_binary(InsertSensor)),

    start_sensor_timer(Sensors),
    {ok, []}.


start_sensor_timer([]) ->
    ok;
start_sensor_timer([Id|Tail]) ->
    gen_server:cast(?MODULE, {start_timer, Id}),       
    start_sensor_timer(Tail).


get_min_max_last_week(SensorId) ->
    try 
	
	%%SELECT MAX(temp), MIN(temp)  FROM `oxelgatan7_weather`  where `sensor`="133" AND `date` BETWEEN DATE_SUB(NOW(), INTERVAL 7 DAY) AND NOW() GROUP BY DATE(`date`)
	Result = emysql:execute(pc_pool, select_min_max_last_week, [SensorId]),
	case Result of
	    {result_packet, _NoOfRows, _ListOfRequest, Rows, _Binary} ->
		Rows;
	    _ ->
		[]
	end	    
    catch
	Ex:Type -> 
	    error_logger:error_msg("~p, ~p, ~p, ~n", [Ex,Type,erlang:get_stacktrace()]),
	    []
    end.
    
get_min_max_today(SensorId) ->
    try 	
	Result = emysql:execute(pc_pool, select_min_max_today, [SensorId]), 
	case Result of
	    {result_packet, _NoOfRows, _ListOfRequest, Rows, _Binary} ->
		Rows;
	    _ ->
		[]
	end
    catch
	Ex:Type -> 
	    error_logger:error_msg("~p, ~p, ~p, ~n", [Ex,Type,erlang:get_stacktrace()]),
	    []
    end.
    


select() ->
    
    Result = emysql:execute(pc_pool,
			    <<"select * from pc.temp order by id desc limit 1">>),
    error_logger:info_msg("~n~p~n", [Result]).

get_time_str({{_,_,_},{H, M, S}}) ->
    io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).

get_date_str({{Y, M, D}, {_,_,_}}) ->    
    io_lib:format('~4..0b-~2..0b-~2..0b', [Y, M, D]).

upload(Id) ->

    try 
	
	Now = calendar:local_time(),
	Date = get_date_str(Now),
	Time = get_time_str(Now),
	
	{sensor, _, Value, _Prev, _Min, _Max, _Date, _Upd} = pc_server:get_sensor(Id),

	emysql:execute(pc_pool, insert_sensor, [Id, 
							    lists:flatten(Date), 
							    lists:flatten(Time),
							    Value])
	    
    catch
	Ex:Type ->
	    error_logger:error_msg("~p, ~p, ~p, ~n", [Ex,Type,erlang:get_stacktrace()])
    end,
    ok.


%% callbacks
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({start_timer, Id}, State) -> 
    timer:send_after(1*60*1000, self(), {timeout, Id}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, Id}, State) ->    
    upload(Id),
    timer:send_after(15*60*1000, self(), {timeout, Id}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
