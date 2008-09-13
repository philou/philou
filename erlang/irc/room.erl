%% @doc Chatting room where multiple chatters speak together.
-module(room).
-export([start/1, connect/3, disconnect/1, tell/2]).

%% @doc Creates a new room.
%% @spec start(string())-> pid()
start(RoomName) ->
    spawn_link(fun() -> loop(new_state(RoomName)) end).

%% @doc Connects a chatter process to an existing room process.
%% @spec connect(pid(), pid(), Nickname) -> ok | { error, Why::string() }
connect(GroupPid, ClientPid, Nickname) ->
    utils:rpc(GroupPid, { connect, ClientPid, Nickname}).

%% @doc Diconnects the calling chatter process from the room.
%% @spec disconnect(pid()) -> ok
disconnect(GroupPid) ->
    utils:send(GroupPid, {disconnect}).

%% @doc Should be called by a chatter when he wants to say something to the room.
%% @spec tell(pid(), string()) -> ok
tell(GroupPid, Message) ->
    utils:send(GroupPid, { said, Message}).

loop(State) ->
    receive
	{ From, { connect, ClientPid, Nickname}} ->
	    case nickname_exists(Nickname, State) of
		true -> 
		    utils:send(From, { error, "Nickname already exists" }),
		    loop(State);
		false ->
		    utils:send(From, ok),
		    client:listen(ClientPid, "Welcome to room " ++ room_name(State)),
		    broadcast(Nickname ++ " joined the room.", State),
		    loop(add_client(ClientPid,Nickname,State))
	    end;
	{ From, {disconnect}} ->
	    Nickname = nickname(From, State),
	    RemainingState = remove_client(From, State),
	    broadcast( Nickname ++ " left the room.", RemainingState),
	    case clients_count(RemainingState) of
		0 -> exit("Everybody left the room.");
		_ -> loop(RemainingState)
	    end;
	{ From, { said, Message}} ->
	    Nickname =  nickname(From, State),
	    io:format("~p said ~p, broadcasting to ~p~n", [Nickname, Message, clients_names(State)]),
	    broadcast( Nickname ++ " said " ++ Message, State),
	    loop(State)
    end.

broadcast(Message, State) ->
    foreach_client_pid(fun(Pid) ->
			  client:listen(Pid, Message) end, State).

%% state handling functions
new_state(RoomName) ->
    {RoomName, dict:new(), dict:new()}.
room_name({RoomName, _ClientPids2Names, _Names2ClientPids}) ->
    RoomName.
nickname_exists(Nickname, {_RoomName, _ClientPids2Names, Names2ClientPids}) ->
    case dict:find(Nickname, Names2ClientPids) of
	{ok, _Value} -> true;
	error -> false
    end.
nickname(ClientPid, {_RoomName, ClientPids2Names, _Names2ClientPids}) ->
    dict:fetch(ClientPid, ClientPids2Names).
add_client(ClientPid, Nickname, {RoomName, ClientPids2Names, Names2ClientPids}) ->
    {RoomName, dict:store(ClientPid, Nickname, ClientPids2Names), dict:store(Nickname, ClientPid, Names2ClientPids)}.
remove_client(ClientPid, {RoomName, ClientPids2Names, Names2ClientPids} = State) ->
    Nickname = nickname(ClientPid, State),
    {RoomName, dict:erase(ClientPid, ClientPids2Names), dict:erase(Nickname, Names2ClientPids)}.
foreach_client_pid(Fun, {_RoomName, ClientPids2Names, _Names2ClientPids}) ->
    dict:map(fun(Pid, _Nickname) ->
		     Fun(Pid) end, ClientPids2Names).
clients_count({_RoomName, ClientPids2Names, _Names2ClientPids}) ->
    dico_length(ClientPids2Names).
clients_names({_RoomName, ClientPids2Names, _Names2ClientPids}) ->
    dico_values(ClientPids2Names).


%% dictionary helper functions
dico_values(Dictionary) ->
    dict:fold(fun(_Key, Value, Acc) -> [ Value | Acc] end, [], Dictionary).

dico_length(Dictionary) ->
    dict:fold(fun(_Key, _Value, Acc) -> Acc + 1 end, 0, Dictionary).
