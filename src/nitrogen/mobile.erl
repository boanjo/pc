%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(mobile).
-include_lib("nitrogen_core/include/wf.hrl").
-include("pc.hrl").
-compile(export_all).

-author("Anders Johansson (epkboan@gmail.com)").

xmlencode([], Acc) -> Acc; 
xmlencode([$<|T], Acc) -> xmlencode(T, Acc ++ "&lt;"); % euro symbol
xmlencode([$å|T], Acc) -> xmlencode(T, Acc ++ "&aring;");
xmlencode([$ä|T], Acc) -> xmlencode(T, Acc ++ "&auml;");
xmlencode([$ö|T], Acc) -> xmlencode(T, Acc ++ "&ouml;");
xmlencode([$Å|T], Acc) -> xmlencode(T, Acc ++ "&Aring;");
xmlencode([$Ä|T], Acc) -> xmlencode(T, Acc ++ "&Auml;");
xmlencode([$Ö|T], Acc) -> xmlencode(T, Acc ++ "&Ouml;");
xmlencode([226,130,172|T], Acc) -> xmlencode(T, Acc ++ "&#8364;");
xmlencode([OneChar|T], Acc) -> xmlencode(T, lists:flatten([Acc,OneChar])). 


main() -> #template{file="./priv/templates/mobile.html"}.

title() -> "Pond Control".

footer() -> 
    {{Year,_Month,_Day},{_Hour,_Min,_Sec}} = erlang:localtime(),
    "&copy; " ++ integer_to_list(Year) ++ " - Anders Johansson".

count(Counter, Pid) ->
    
    case Pid of
	0 ->
	    pc_server:add_listener(self());
	
	_ ->
	    ok
    end,
    
%%     io:format("Pid ~p~n", [Pid]),

    receive
	{temp, Value} ->    
	    wf:update(tempspan, wf:to_list(Value)),
	    wf:flush();
	{flow, Value} ->    
	    wf:update(flowspan, wf:to_list(Value)),
	    wf:flush();
	{ph, Value} ->    
	    wf:update(phspan, wf:to_list(Value)),
	    wf:flush();
	{level, Value} ->    
	    wf:update(levelspan, wf:to_list(Value)),
	    wf:flush();
	_Else ->
	    ok
    end,
						% flush commands to browser
    ?MODULE:count(Counter + 1, self()).

map_state(State) ->
   case State of
       on ->
	   "on";
       _ ->
	   "off"
   end.


body() ->

    wf:comet(fun()->
	count(0, 0) end),
    
    {ok, WantedDeviceList} = application:get_env(pc, device),

    Sensors = 
    [
	"Temp:   ",#span{style="font-size: 35px; font-weight: bold;", id=tempspan},
	#p{},
	"PH :    ",#span{style="font-size: 35px; font-weight: bold;", id=phspan},
	#p{},
	"Level : ",#span{style="font-size: 35px; font-weight: bold;", id=levelspan},
	#p{},
	"Flow :  ",#span{style="font-size: 35px; font-weight: bold;", id=flowspan},
	#p{}
    ],
    
    Devices = create_device(WantedDeviceList, []),

    Sensors ++ Devices.

add_alarm_cell(_Id, []) ->
    [];
add_alarm_cell(Id, _AlarmList) ->
    Element = #tablecell { 
		 align=right,
		 valign=bottom,
		 body =[
			#button { 
			   id=list_to_atom("button_" ++ atom_to_list(Id)),
			   click=#toggle{target=list_to_atom("alarm_menu" ++ atom_to_list(Id))},
			%%			   postback={click, list_to_atom("button_" ++ integer_to_list(Id)), Id} ,
			   data_fields=[{icon, 'arrow-d'},
			   		{mini, true},
			   		{inline, true},
			   		{theme, c},
			   		{iconpos, notext}]
			  }
		       ]},
    Element.



add_alarm_menu(_Id, []) ->
    [];
add_alarm_menu(Id, AlarmList) ->
    Element = 	#mobile_list{
		   id=list_to_atom("alarm_menu" ++ atom_to_list(Id)),
		   theme=a,
		   inset=true,
		   style="display:none",
		   body=[#mobile_list_divider{class=c, text="Timers"}] ++ create_alarms(AlarmList, [])
		  },
    Element.

create_alarms([], Acc) ->
    Acc;

create_alarms([Head|Tail], Acc) ->

    {Cmd, {H,M,S}} = Head,
    Element = #mobile_listitem{
		 theme=c,
		 body=   #mobile_grid { 
			    columns=2,
			    blocks=[
				    #mobile_grid_block{ text=Cmd },
				    #mobile_grid_block{ text=io_lib:format("at ~p:~p:~p", [H,M,S])}]
			   }
		},
    
    create_alarms(Tail, [Element|Acc]).

create_device([], Acc) ->
    Acc;

create_device([Head|Tail], Acc) ->

    {Id, Name, AlarmList} = Head,   

    Device = pc_server:get_device(Id),

    NewName = xmlencode(Name, []),

    ListElement = [#label { text=NewName, html_encode=false },
		   #table { rows=[
				  #tablerow { 
				     cells=[
					    #tablecell { 
					       body =[
						      #mobile_toggle{
							 on_text="on",
							 off_text="off",

							 selected=map_state(Device#device.state),
							 postback={button, Id},
							 id=Id,
							 width=100
							}]},
					    add_alarm_cell(Id, AlarmList),
					    add_alarm_menu(Id, AlarmList)
					    
					   ]
				    }
				 ]
			  },
		   
		   #hr{}],
    create_device(Tail, [ListElement|Acc]).

event({button, Id}) 
  when is_atom(Id)->
    SendCmd = wf:q(Id),
    pc_server:device(Id, list_to_atom(SendCmd));

event({click, Button, Id}) ->
    ShowMenu = wf:q(Button),
    
    Menu = wf:q(list_to_atom("alarm_menu" ++ integer_to_list(Id))),
    error_logger:info_msg("ShowMenu ~p for button ~p menu ~p~n", [ShowMenu, Button, Menu]),
    case ShowMenu of
        "on" -> wf:wire(list_to_atom("alarm_menu" ++ integer_to_list(Id)),#slide_down{});
        _Other -> wf:wire(list_to_atom("alarm_menu" ++ integer_to_list(Id)),#slide_up{})
    end;

event(Other) ->    
    error_logger:info_msg("Other ~p~n", [Other]).

