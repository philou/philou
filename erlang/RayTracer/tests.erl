-module(tests).
-export([test_draw/0]).
-import(raytracer_impl, [point_on_screen/3, vector_to_screen/3, ray_color/2, ray_x_polynom/1, ray_y_polynom/1, ray_z_polynom/1, distance_to_sphere/2]).
-import(vectors, [x/1,y/1,z/1]).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% This is a convenience macro which gives more detailed reports when
%% the expected LHS value is not a pattern, but a computed double value.
%% The test succeedes if the value is close enough with expectation.
-ifdef(NOASSERT).
-define(assertClose(Expect,Expr),ok).
-else.
-define(assertClose(Expect, Expr),
	((fun (__X) ->
	    __V = Expr,
	    case (abs(__V - __X) < ?EPSILON) of
		true -> ok;
		false -> .erlang:error({assertEqual_failed,
				      [{module, ?MODULE},
				       {line, ?LINE},
				       {expression, (??Expr)},
				       {expected, __X},
				       {value, __V}]})
	    end
	  end)(Expect))).
-endif.
-define(_assertClose(Expect, Expr), ?_test(?assertClose(Expect, Expr))).


-define(TEST_PPM, "test.ppm").



%% image tests
a_picture(Size) ->
    List = lists:seq(0,Size-1,1),
    [[#color{r=X/Size,g=Y/Size,b=Y/Size} || X <- List] || Y <- List].

a_grey_picture(Size) ->
    List = lists:seq(0,Size-1,1),
    [[#color{r=(X+Y)/(2*Size),g=(X+Y)/(2*Size),b=(X+Y)/(2*Size)} || X <- List] || Y <- List].

a_monochrome(#size{width=Width,height=Height},Color) ->
    [[Color || _X <- lists:seq(0,Width-1)] || _Y <- lists:seq(0,Height-1)].

some_ppm_data(Size) ->
    image:build_ppm_data(a_picture(Size)).
    
save_load_ppm_data(PPMData,Filename) ->
    image:save_ppm_data(PPMData,Filename),
    image:load_ppm_data(Filename).

image_test_() ->
    [?_assertEqual(0, image:height([])),
     ?_assertEqual(0, image:width([])),
     ?_assertEqual(10, image:width(a_picture(10))),
     ?_assertEqual(10, image:height(a_picture(10))),
     ?_assertEqual([], image:matrix_to_list([])),
     ?_assertEqual([1,2,3], image:matrix_to_list([[1,2,3]])),
     ?_assertEqual([1,2,3,4,5,6], image:matrix_to_list([[1,2,3],[4,5,6]])),
     ?_assertEqual(#color{r=0,g=0,b=255}, image:float_to_int_color(255,#color{r=0.0,g=0.0,b=1.0})),
     ?_assertEqual(some_ppm_data(3), save_load_ppm_data(some_ppm_data(3), ?TEST_PPM)),
     ?_assertNot(some_ppm_data(4) == save_load_ppm_data(some_ppm_data(3), ?TEST_PPM))
     ].

%% raytracer integration tests
raytracer_test_() ->
    [?_assertEqual(a_monochrome(?SMALL,?WHITE), raytracer:render(?SMALL, #scene{}))
    ,?_assertEqual(a_monochrome(?SMALL,?MAGENTA), raytracer:render(?SMALL, #scene{background=?MAGENTA}))
    ,?_assertEqual(a_monochrome(?MEDIUM,?WHITE), raytracer:render(?MEDIUM, #scene{}))
    ,?_assertNot(a_monochrome(?SMALL,?WHITE) == raytracer:render(?SMALL, #scene{objects=[#sphere{radius=0.5}]}))
     ].

%% raytracer unit tests
raytracer_impl_test_() ->
    [?_assertEqual([[2,4],[10,14]], raytracer:map2(fun(Int) -> 2*Int end, [[1,2],[5,7]]))
    ,?_assertEqual(?ORIGIN, point_on_screen(0,0,#size{width=1,height=1}))
    ,?_assert(in_top_left(point_on_screen(0,0,?SMALL)))
    ,?_assert(x(point_on_screen(0,0,?SMALL)) < x(point_on_screen(1,0,?SMALL)))
    ,?_assert(y(point_on_screen(0,1,?SMALL)) < y(point_on_screen(0,0,?SMALL)))
    ,?_assertEqual(z(point_on_screen(0,0,?SMALL)), z(point_on_screen(1,1,?SMALL)))
    ,?_assert(x(vector_to_screen(0,0,?SMALL)) < x(vector_to_screen(1,0,?SMALL)))
    ,?_assert(y(vector_to_screen(0,1,?SMALL)) < y(vector_to_screen(0,0,?SMALL)))
    ,?_assert(0 =< z(vector_to_screen(0,0,?SMALL)))
    ,?_assertEqual([-1.0,1.0], ray_z_polynom(#ray{origin=#coord{z=-1.0}}))
    ,?_assertEqual([], ray_x_polynom(#ray{origin=#coord{z=-1.0}}))
    ,?_assertEqual([0.0,1.0], ray_y_polynom(#ray{origin=#coord{z=-1.0},direction=#vector{y=1.0}}))
    ,?_assertEqual(?WHITE, ray_color(#ray{origin=#coord{z=-1}}, #scene{}))
    ,?_assert(distance_to_sphere(#ray{origin=#coord{z=-1}, direction=vectors:normalize(#vector{x=0.1,y=0.1,z=1})},
				 #sphere{radius=0.5}) < 1.0)
    ,?_assertClose(2.3, distance_of_point_at_distance(2.3))
    ,?_assertClose(2.3, point_at_distance_dot_direction(2.3))
    ,?_assertEqual(vectors:normalize(#vector{x=1,y=1,z=1}), raytracer_impl:normal_at_sphere(#coord{x=1,y=1,z=1},
											     #sphere{radius=math:pow(3,1/3)}))
    ,?_assertEqual(?RED, ray_color(#ray{origin=#coord{z=-1}},
				   #scene{objects=[#sphere{radius=0.5,
							   texture=#texture{color=?RED,
								    ambient=1.0}}]}))
    ,?_assertEqual(?CYAN, ray_color(#ray{origin=#coord{z=-1}},
				    #scene{objects=[#sphere{radius=0.5,
							    texture=#texture{color=?CYAN,
								     ambient=1.0}}]}))
    ,?_assertEqual(?WHITE, ray_color(#ray{origin=#coord{z=-1}},
				     #scene{objects=[#sphere{center=#coord{x=10},
							     radius=0.5,
							     texture=#texture{color=?RED,
									      ambient=1.0}}]}))
    ,?_assertEqual(colors:scale(0.37,?RED), ray_color(#ray{origin=#coord{z=-1}},
						      #scene{objects=[#sphere{radius=0.5,
									      texture=#texture{color=?RED,
											       ambient=0.37}}]}))
    ,?_assertNot(?RED == ray_color(#ray{origin=#coord{z=-1}},
				   #scene{objects=[#sphere{radius=0.5,texture=#texture{color=?RED}}],
					  lights=[#coord{x=1,y=1,z=-1}]}))
    ,?_assertNot(?BLACK == ray_color(#ray{origin=#coord{z=-1}},
				     #scene{objects=[#sphere{radius=0.5,texture=#texture{color=?RED}}],
					    lights=[#coord{x=1,y=1,z=-1}]}))
     ].

in_top_left(#coord{x=X,y=Y,z=Z})->
    (X < 0) and (0 < Y) and (Z==0).

distance_of_point_at_distance(Distance) ->
    Origin = #coord{z=-1},
    Ray = #ray{origin=Origin,direction=vectors:normalize(#vector{x=0.1,y=0.1,z=1})},
    Point = raytracer_impl:point_at_distance(Ray, Distance),
    Vector = vectors:from_coords(Origin, Point),
    vectors:norm(Vector).

point_at_distance_dot_direction(Distance) ->
    Origin = #coord{z=-1},
    Ray = #ray{origin=Origin,direction=vectors:normalize(#vector{x=0.1,y=0.1,z=1})},
    Point = raytracer_impl:point_at_distance(Ray, Distance),
    Vector = vectors:from_coords(Origin, Point),
    vectors:dot(Vector,Ray#ray.direction).

%% polynoms tests
polynoms_test_() ->
    [?_assertEqual([1,2,3],polynoms:add([1,1,2],[0,1,1]))
    ,?_assertEqual([1,2,3],polynoms:add([0],[1,2,3]))
    ,?_assertEqual([1,2,3],polynoms:add([0,2,3], [1]))
    ,?_assertEqual([2,2,4],polynoms:add([[1,1,2],[0,1,1],[1,0,1]]))
    ,?_assertEqual([2,4,6],polynoms:scale(2,[1,2,3]))
    ,?_assertEqual([0,1,2,3],polynoms:mult_by_x([1,2,3]))
    ,?_assertEqual([1,1],polynoms:mult([1],[1,1]))
    ,?_assertEqual([-1,0,1],polynoms:mult([1,1],[-1,1]))
    ,?_assertEqual([1],polynoms:pow([1,1],0))
    ,?_assertEqual([1,1],polynoms:pow([1,1],1))
    ,?_assertEqual([1,2,1],polynoms:pow([1,1],2))
    ,?_assertEqual([0.5],polynoms:roots([-1,2]))
    ,?_assertEqual([1.0,2.0],polynoms:roots([2,-3,1]))
    ,?_assertEqual([],polynoms:reduce([0]))
    ,?_assertEqual([1],polynoms:reduce([1,0]))
    ,?_assertEqual([1,2,3],polynoms:reduce([1,2,3,0,0]))
    ,?_assertEqual(2.0,polynoms:eval([2],3.7))
    ,?_assertEqual(2.0,polynoms:eval([2,1],0.0))
    ,?_assertEqual(4.0,polynoms:eval([2,1],2.0))
    ,?_assertEqual(57.0,polynoms:eval([1,1,1],7.0))
    ].

%% vectors tests
vectors_test_() ->
    [?_assertEqual(1.0,vectors:norm(#vector{x=1,y=0,z=0}))
    ,?_assertEqual(#vector{x=1.0,y=0.0,z=0.0},vectors:normalize(#vector{x=2,y=0,z=0}))
     ].

test_draw() ->
%    image:save(a_grey_picture(200),?TEST_PPM).
    raytracer:render(?MEDIUM,#scene{objects=[#sphere{radius=0.5}],lights=[#coord {x=1,y=1,z=-2}]},?TEST_PPM).

