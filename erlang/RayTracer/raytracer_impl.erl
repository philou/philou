%% @doc Implementation for the raytracer module.
-module(raytracer_impl).
-compile(export_all).
-import(vectors, [move/2, from_coords/2]).
-import(polynoms, [add/1,pow/2,scale/2, roots/1]).
-include("records.hrl").

-define(CAMERA, #coord{z=-1.0}).
-define(LOOK_AT, ?ORIGIN).
-define(TOP_LEFT, #coord{x=-1,y=0.625,z=0}).
-define(H_VECTOR, #vector{x=2}).
-define(V_VECTOR, #vector{y=-1.25}).

pixel_color(NH,NV,Size,Scene)->
    ray_color(ray(NH,NV,Size),Scene).

ray_color(_Ray,#scene{background=BackgroundColor,objects=[],lights=_Lights})->
    BackgroundColor;
ray_color(Ray,#scene{background=BackgroundColor,objects=[#sphere{texture=#texture{color=Color, ambient=Ambient}}=Sphere],lights=Lights}) ->
    case distance_to_sphere(Ray,Sphere) of
	infinity -> BackgroundColor;
	Distance -> colors:scale(Ambient + (1.0-Ambient)*diffuse_shade(Ray,Distance,Sphere,Lights),Color)
    end.

diffuse_shade(_Ray,_Distance,_Sphere,[]) ->
    0.0;
diffuse_shade(Ray,Distance,Sphere,[Light]) ->
    Point = point_at_distance(Ray,Distance),
    L = vectors:normalize(vectors:from_coords(Point,Light)),
    N = normal_at_sphere(Point,Sphere),
    lists:max([0.0, vectors:dot(L,N)]).
    
ray(NH,NV,Size) ->
    #ray{origin=?CAMERA, direction=vector_to_screen(NH,NV,Size)}.

vector_to_screen( NH, NV, Size) ->
    vectors:normalize(vectors:from_coords(?CAMERA,point_on_screen(NH,NV,Size))).

point_on_screen( NH, NV, #size{width=Width,height=Height}) ->
    HFactor = (NH + 0.5) / Width,
    VFactor = (NV + 0.5) / Height,
    move(move(?TOP_LEFT,vectors:scale(HFactor,?H_VECTOR)), vectors:scale(VFactor,?V_VECTOR)).

distance_to_sphere(Ray,Sphere) ->
    case roots(polynom_to_sphere(Ray,Sphere)) of
	[] -> infinity;
	Roots -> lists:min(Roots)
    end.
  
polynom_to_sphere(Ray, #sphere{center=#coord{x=Xs,y=Ys,z=Zs},radius=R}) ->
    PolX = ray_x_polynom(Ray),
    PolY = ray_y_polynom(Ray),
    PolZ = ray_z_polynom(Ray),
    add([pow(PolX,2), pow(PolY,2), pow(PolZ,2),
	 scale(2*Xs,PolX), scale(2*Ys,PolY), scale(2*Zs,PolZ),
	 [Xs*Xs + Ys*Ys + Zs*Zs - R*R]]).

normal_at_sphere(Point,#sphere{center=Center}) ->
    vectors:normalize(vectors:from_coords(Center,Point)).

point_at_distance(Ray, Distance) ->
    #coord{x=polynoms:eval(ray_x_polynom(Ray),Distance),
	   y=polynoms:eval(ray_y_polynom(Ray),Distance),
	   z=polynoms:eval(ray_z_polynom(Ray),Distance)}.

ray_x_polynom(#ray{origin=#coord{x=I},direction=#vector{x=D}}) ->
    polynoms:reduce([I,D]).
ray_y_polynom(#ray{origin=#coord{y=I},direction=#vector{y=D}}) ->
    polynoms:reduce([I,D]).
ray_z_polynom(#ray{origin=#coord{z=I},direction=#vector{z=D}}) ->
    polynoms:reduce([I,D]).
