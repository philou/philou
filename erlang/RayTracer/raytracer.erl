%% @doc Enty point for our ray-tracer.
-module(raytracer).
-export([render/2, render/3, map2/2]).
-include("records.hrl").

%% @doc Renders a scene to a file.
%% @spec render(size(), scene(), string()) -> ok | Error
render(Size, Scene, Filename) ->
    image:save(render(Size,Scene),Filename).

%% @doc Renders a scene to a pixel matrix.
%% @spec render(size(),scene()) -> pixels()
render(#size{width=Width,height=Height}=Size, Scene) ->
    render_pixels([[{NH,NV,Size,Scene} || NH <- lists:seq(0, Width-1)] || NV <- lists:seq(0,Height-1)]).

render_pixels(PixelArgs) ->
    WorkersPids = spawn_pixels(PixelArgs),
    collect_pixels(WorkersPids).

spawn_pixels(PixelArgs) ->
    map2(fun spawn_pixel/1, PixelArgs).

spawn_pixel({NH,NV,Size,Scene}) ->
    Self = self(),
    spawn(fun() ->
		  Self ! {self(), raytracer_impl:pixel_color(NH,NV,Size,Scene)} end).

collect_pixels(WorkerPids) ->
    map2(fun collect_pixel/1, WorkerPids).

collect_pixel(WorkerPid) ->
    receive
	{WorkerPid, Color} -> Color
    end.

%% @doc Maps a function over a list of list
%% @spec map2(fun(), list(list())) -> list(list())
map2(Fun, ListOfList) ->
    lists:map(fun(List) ->
		      lists:map(Fun,List) end, ListOfList).
