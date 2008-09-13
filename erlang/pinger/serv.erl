-module(serv).
-export([start/0, ping/0, ping/1, pinger_pid/0, pinger_pid/1, touch/0]).

start() ->
    register(the_pinger, spawn(fun loop/0)).

ping() ->
    ping(the_pinger).

pinger_pid(RemoteHost) ->
    rpc:call(RemoteHost, serv, pinger_pid, []).
    
pinger_pid() ->
    whereis(the_pinger).

loop() ->
    receive
	{ From, ping} ->
	    io:format("server was pinged by ~p.~n", [From]),
	    From ! pinged,
	    loop()
    end.

ping(ServerPid) ->
    ServerPid ! { self(), ping },
    receive
	pinged ->
	    true
    end.

touch() ->
    io:format("Huuuu ... scary !~n").
