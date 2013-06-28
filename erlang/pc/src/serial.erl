-module(serial).
-export([read_serial/0]).
-export([read_serial/2]).
-export([handle_serial/2]).
-export([send_serial/1]).

-include_lib("/home/pi/wpi/include/wpi.hrl").

send_serial(Msg) ->
    serial ! {send, Msg}.

read_serial() ->
    read_serial("/dev/ttyACM0", 9600).

read_serial(Str, Baud) ->
    
    gen_event:start({local, serial_monitor}),
    gen_event:add_handler(serial_monitor, serial_terminal_logger, []),
    gen_event:add_handler(serial_monitor, serial_file_logger, ["/home/pi/serial.txt"]),

    Handle = wpi:serial_open(Str, Baud),
    case Handle of
        -1 ->
            erlang:error(could_not_open_serial_port);
        _ ->
            Pid = spawn(serial, handle_serial, [[], Handle]),
	    register(serial, Pid)
		
	    %%            wpi:serial_close(Handle)
    end.

put_char(Handle, [H|T]) ->    
    wpi:serial_put_char(Handle, H),
    put_char(Handle, T);
put_char(Handle, []) ->
    NewLine = $\n,
    wpi:serial_put_char(Handle, NewLine),
    ok.
    

handle_serial(Str, Handle) ->
    
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
            handle_serial(Str, Handle);
        -1 ->
            %%io:format("Serial Port is lost ~p", [Str]);
	    error_logger:warning_msg("Serial Port Closed while Reading, Handle: ~p~n", [Handle]),
	    exit(serial_port_down_while_reading);

        _ ->
	    Char = wpi:serial_get_char(Handle),
	    case Char of
		$\r ->
		    handle_serial(Str, Handle);
		$\n ->
		    gen_event:notify(serial_monitor, lists:reverse(Str)),
		    handle_serial([], Handle);
		_ ->
		    %%            io:format("~c", [wpi:serial_get_char(Handle)]),
		    handle_serial([Char|Str], Handle)
	    end
    end.



