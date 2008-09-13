%% @doc Simple utils for our simple chat system.
-module(utils).
-export([rpc/2, send/2, receive_from/1]).

%% @doc Sends a request to Pid, waits to get an answer and returns it. Inter process messages are wrapped
%% in a {self(), Message } way.
%% @spec rpc(pid(), Request) -> Result
rpc(Pid, Request) ->
    send(Pid, Request),
    receive_from(Pid).

%% @doc Sends { self(), Message} to a process.
%% @spec send(pid(), Message) -> ok
send(Pid, Message) ->
    Pid ! { self(), Message },
    ok.

%% @doc Receives a { Pid, Message } and returns Message.
%% @spec receive_from(pid()) -> Message
receive_from(Pid) ->
    receive
	{Pid, Message } ->
	    Message
    end.
