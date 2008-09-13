-module(helloworld_tk).
-export([start/0]).

start() ->
    Gs = gs:start(),
    Win = gs:create(window,Gs,[{width,200},{height,100}]),
    gs:create(label,Win,[{label, {text, "Hello, world !"}}]),
    gs:config(Win, {map, true}),
    loop().

loop()->
    receive
	Event ->
	    io:format("An event was received : ~p~n", [Event])
    end.
