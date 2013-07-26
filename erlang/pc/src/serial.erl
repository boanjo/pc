-module(serial).
-export([start/0, start/2]).
-export([loop/2]).
-export([send/1]).
-export([terminal/1]).

-include_lib("/home/pi/wpi/include/wpi.hrl").

send(Msg) ->
    serial ! {send, Msg}.

start() ->
    start("/dev/ttyACM0", 9600).

terminal(Bool) when Bool =:= on ->
    gen_event:add_handler(serial_monitor, serial_terminal_logger, []);

terminal(_) ->
    gen_event:delete_handler(serial_monitor, serial_terminal_logger, []).


start(Str, Baud) ->
    
    gen_event:start({local, serial_monitor}),
%%    gen_event:add_handler(serial_monitor, serial_terminal_logger, []),
    gen_event:add_handler(serial_monitor, serial_file_logger, ["/home/pi/serial.txt"]),
    
    Handle = wpi:serial_open(Str, Baud),
    case Handle of
        -1 ->
            erlang:error(could_not_open_serial_port);
        _ ->
            Pid = spawn(?MODULE, loop, [[], Handle]),
	    register(serial, Pid)
    end.

put_char(Handle, [H|T]) ->    
    wpi:serial_put_char(Handle, H),
    put_char(Handle, T);

put_char(Handle, []) ->
    NewLine = $\n,
    wpi:serial_put_char(Handle, NewLine),
    ok.
    

notify_mysql(Msg) ->
    case whereis(mysql_handler) of % Test if the client is running
        undefined ->
            no_mysql;
        _ -> 
	    mysql_handler ! {serial_msg, Msg},
	    ok
    end.

loop(Str, Handle) ->
    
    receive 
	{send, Msg} ->
	    put_char(Handle, Msg)
    after
	0 ->
	    true
    end,
    
    AvailableBytes = wpi:serial_data_avail(Handle),
    case AvailableBytes of
        0 ->
            loop(Str, Handle);
        -1 ->
            %%io:format("Serial Port is lost ~p", [Str]);
	    error_logger:warning_msg("Serial Port Closed while Reading, Handle: ~p~n", [Handle]),
	    exit(serial_port_down_while_reading);

        _ ->
	    Char = wpi:serial_get_char(Handle),
	    case Char of
		$\r ->
		    loop(Str, Handle);
		$\n ->
		    Rev = lists:reverse(Str),
		    gen_event:notify(serial_monitor, Rev),
		    notify_mysql(Rev),
		    loop([], Handle);
		_ ->
		    %%            io:format("~c", [wpi:serial_get_char(Handle)]),
		    loop([Char|Str], Handle)
	    end
    end.



