-module(serial).
-export([set/1, read_serial/2]).
-export([pulse/2]).
-export([monitor/1]).
-include_lib("include/wpi.hrl").

pulse(Pin, Cnt) ->
    wpi:pin_mode(Pin, output),
    toggle(Pin, Cnt).

toggle(_Pin, 0) ->
    ok;

toggle(Pin, Cnt) ->
    wpi:digital_write(Pin, 0),
    timer:sleep(20),
    wpi:digital_write(Pin, 1),
    timer:sleep(1),
    toggle(Pin, Cnt-1).

read_serial(Str, Baud) ->
    Handle = wpi:serial_open(Str, Baud),
%%    start_monitor(Str),
    case Handle of
        -1 ->
            erlang:error(could_not_open_serial_port);
        _ ->
            read(Str, Handle),
            wpi:serial_close(Handle)
    end.

start_monitor(Str) ->
    Pid = spawn(serial, monitor, [Str]),
    Pid ! timer.

monitor(Str) ->
    receive
        timer ->
            Res = file:read_file_info(Str),
            io:format("Serial Port down!!!! ~p", [Res]),
            %% case Res of
            %%  error ->
            %%      io:format("Serial Port down!!!!", []);
            %%  _ ->
            %%      ok
            %% end,
            timer:send_after(1000, timer);
        _ ->
            io:format("Unknown message", [])
    end,
    monitor(Str).




read(Str, Handle) ->

    AvailableBytes = wpi:serial_data_avail(Handle),
    case AvailableBytes of
        0 ->
            read(Str, Handle);
        -1 ->
            io:format("Serial Port is lost ~p", [Str]);

        _ ->
            io:format("~c", [wpi:serial_get_char(Handle)]),
            read(Str, Handle)
    end.



set(Val) ->
    Pin = 7,
    wpi:pin_mode(Pin, output),
    wpi:digital_write(Pin, Val).
