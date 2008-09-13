%% @doc Communication dialog for a chatter.
-module(gui).
-export([start/2, display/2]).

%% @doc Shows the dialog using the calling process as client communication agent.
%% @spec start(string(),string())-> pid()
start(Nickname, RoomName) ->
    ClientPid = self(),
    gs:start(),
    spawn_link(fun() -> widget(ClientPid, Nickname, RoomName) end).

%% @doc Displays text in the main chat backlog.
%% @spec display(pid(),string()) -> ok
display(GuiPid, Text) ->
    utils:send(GuiPid, { display, Text}).

widget(ClientPid, Nickname, RoomName) ->
    Size = [{width,500},{height,200}],
    Win = gs:window(gs:start(),
		    [{map,true},{configure,true},{title,Nickname ++ "@" ++ RoomName}|Size]),
    gs:frame(packer, Win,[{packer_x, [{stretch,1,500}]},
			  {packer_y, [{stretch,10,120,100},
				      {stretch,1,15,15}]}]),
    gs:create(editor,editor,packer, [{pack_x,1},{pack_y,1},{vscroll,right}]),
    gs:create(entry, entry, packer, [{pack_x,1},{pack_y,2},{keypress,true}]),
    gs:config(packer, Size),
    Prompt = " > ",
    gs:config(entry, {insert,{0,Prompt}}),
    loop({ClientPid,Prompt}).

loop({ClientPid,Prompt} = State) ->
    receive
	{gs, entry, keypress, [], ['Return', _, _, _]} ->
	    Text = gs:read(entry, text),
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0, Prompt}}),
	    client:tell(ClientPid,Text),
	    loop(State);
	{gs, _, configure, [], [Width, Height, _, _]} ->
	    gs:config(packer, [{width, Width},{height, Height}]),
	    loop(State);
	{gs, _, destroy, _, _}->
	    exit("user closed the window");
	{ClientPid, {display, Text}} ->
	    gs:config(editor, {insert,{'end',Text}}),
	    gs:config(editor, {insert,{'end',"\n"}}),
	    loop(State);
	Any ->
	    io:format("Un evenement a été ignoré par l'IHM : ~p~n", [Any]),
	    loop(State)
    end.

