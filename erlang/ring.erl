-module(ring).
-compile(export_all).

start(N) ->
    Processes = create_processes(N),
    init_processes(Processes),
    first(Processes).

create_processes(0) -> [];
create_processes(N) ->
    Pid = spawn(fun() -> start_process() end),
    [ Pid | create_processes(N-1) ].

start_process() ->
    receive
	{init, PreviousPid, NextPid} ->
	    loop(PreviousPid, NextPid)
    end.

loop(PreviousPid, NextPid) ->
    Self = self(),
    receive
        {origin, Self, PreviousPid, Message} ->
	    io:format("~p received ~p back from the ring !~n", [Self, Message]),
	    loop(PreviousPid, NextPid);
	{origin, Pid, PreviousPid, Message} ->
	    io:format("~p received ~p, broadcasting to ~p~n", [Self, Message, NextPid]),
	    NextPid ! {origin, Pid, Self, Message},
            loop(PreviousPid, NextPid);
	Message ->
	    io:format("~p received incoming message ~p, broadcasting to ~p~n", [Self, Message, NextPid]),
	    NextPid ! {origin, Self, Self, Message},
	    loop(PreviousPid, NextPid)
    end.

init_processes(Processes) -> init_processes_1(ring_list(Processes)).

init_processes_1([PreviousPid, Pid, NextPid | T]) ->
    Pid ! { init, PreviousPid, NextPid},
    init_processes_1([Pid, NextPid | T]);
init_processes_1(_) -> void.



ring_list(L) ->
    [last(L)] ++ L ++ [first(L)].

first([H|_]) -> H.

last([H|[]]) -> H;
last([_H|T]) -> last(T).
		    
