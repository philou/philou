%% @doc Server for our simple chat system.
-module(chat).
-export([start/0, the_chat_pid/0, connect/3]).

%% @doc Starts and registers a chat server.
%% @spec start() -> pid()
start() ->
    register(the_chat, spawn(fun live/0)).

%% @doc Process id of the started chat server.
%% @spec the_chat_pid() -> pid() | undefined
the_chat_pid() ->
    whereis(the_chat).

%% @doc Connects the calling process to a room using a nickname. Create a new room if needed.
%% @todo Handle the case where the nickname is already in use. 
%% @spec connect(pid(), string(), string()) -> {ok, Room::pid()} | {error, Why::string()}
connect(ServerPid, RoomName, Nickname) ->
    utils:rpc(ServerPid, {connect, RoomName, Nickname}).

live() ->
    process_flag(trap_exit, true),
    loop(new_state()).

loop(State)->
    receive
	{ 'EXIT', RoomPid, Why } ->
	    RoomName = room_name(RoomPid, State),
	    io:format("Room ~p closed because ~p.~n", [RoomName, Why]),
	    remove_room(RoomPid, State);
	{ From, { connect, RoomName, Nickname }} ->
	    {RoomPid, NewState} = find_or_create_room(RoomName, State),
	    case room:connect(RoomPid, From, Nickname) of
		ok ->
		    utils:send(From, { ok, RoomPid });
		{ error, Why } ->
		    utils:send(From, { error, Why })
	    end,
	    loop(NewState);
        Any ->
	    io:format("The server ignored unexpected message ~p.~n", [Any]),
	    loop(State)
    end.

find_or_create_room(RoomName, State) ->
    case room_pid(RoomName, State) of
	{ok, RoomPid } ->
	    {RoomPid, State};
	error ->
	    RoomPid = room:start(RoomName),
	    {RoomPid,add_room(RoomPid, RoomName, State)}
    end.

%% state manipulation functions
new_state() ->
   { dict:new(), dict:new()}.
room_name(RoomPid, {_RoomNames2Pids, RoomPids2Names}) ->
    dict:fetch(RoomPid, RoomPids2Names).
room_pid(RoomName, {RoomNames2Pids, _RoomPids2Names}) ->
    dict:find(RoomName, RoomNames2Pids).
remove_room(RoomPid,{RoomNames2Pids, RoomPids2Names} = State) ->
    RoomName = room_name(RoomPid, State),
    loop({dict:erase(RoomName, RoomNames2Pids), dict:erase(RoomPid, RoomPids2Names)}).
add_room(RoomPid, RoomName, {RoomNames2Pids, RoomPids2Names}) ->
    {dict:store(RoomName, RoomPid, RoomNames2Pids), dict:store(RoomPid, RoomName, RoomPids2Names)}.

