-module(spawn_register).
-compile(export_all).

delay_start(Fun) ->
    receive
	really_start ->
	    Fun()
    end.


start(PName, Fun) ->
    Pid = spawn(fun() -> delay_start(Fun) end ),
    register(PName, Pid),
    PName ! really_start.


tick(0) -> true;
tick(N) ->
    receive
    after 1000 ->
	    io:format("Tick ~p ... ~n", [N]),
	    tick(N-1)
    end.
