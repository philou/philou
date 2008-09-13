%% @doc Communication agent for a chatter.
-module(client).
-export([start/2, start_remote/3, tell/2, listen/2]).

%% @doc Adds a new chatter with a nickname in a room. The chat server is supposed to be on the same erlang node.
%% @todo Handle an unreachable server.
%% @spec start(string(), string()) -> ok | { error, Why::string() }
start(Nickname, RoomName) ->
    ServerPid = chat:the_chat_pid(),
    start(Nickname, ServerPid, RoomName).

%% @doc Adds a new chatter with a nickname in a room. The chat server is supposed to be reacheable at RemoteHost.
%% @todo Handle an unreachable server.
%% @spec start_remote(string(), string(), string()) -> ok | { error, Why::string() }
start_remote(Nickname, RemoteHost, RoomName) ->
    ServerPid = rpc:call(RemoteHost, chat, the_chat_pid, []),
    start(Nickname, ServerPid, RoomName).

%% @doc Should be called from the gui process, asks the chatter agent to tell something to the room.
%% @spec tell(pid(),string()) -> ok
tell(ClientPid, Message) ->
    utils:send(ClientPid, { tell, Message}).

%% @doc Should be called from the room process, informs the chatter agent about what is being said in the room.
%% @spec listen(pid(),string()) -> ok
listen(ClientPid, Message) ->
    utils:send(ClientPid, { listen, Message}).

%% spec start(string(), pid(), string()) -> ok | { error, Why::string() }
start(Nickname, ServerPid, RoomName) ->
    Self = self(),
    ClientPid = spawn(fun() ->
		  live(Self, Nickname, ServerPid, RoomName) end),
    utils:receive_from(ClientPid).

live(SpawnerPid, Nickname, ServerPid, RoomName) ->
    process_flag(trap_exit, true),
    case chat:connect(ServerPid, RoomName, Nickname) of
	{ ok, RoomPid } ->
	    utils:send(SpawnerPid, ok ),
	    IOPid = gui:start(Nickname, RoomName),
	    loop({Nickname, IOPid, RoomPid});
	{ error, Why } ->
	    utils:send(SpawnerPid, { error, Why })
    end.

loop({Nickname, IOPid, RoomPid} = State) ->
    receive
	{ 'EXIT', IOPid, Why} ->
	    io:format("Client ~p is terminating because ~p.~n", [Nickname, Why]),
	    room:disconnect(RoomPid);
	{ IOPid, { tell, Message}} ->
	    room:tell(RoomPid, Message),
	    loop(State);
	{ RoomPid, { listen, Message}} ->
	    gui:display(IOPid, Message),
	    loop(State);
	Any ->
	    io:format("~p received an unexpected message ~p.~n", [Nickname, Any]),
	    loop(State)
    end.
