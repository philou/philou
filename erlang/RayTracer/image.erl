%% @doc Image file save and load. Handles .ppm files.
-module(image).
-export([save/2, width/1, height/1, matrix_to_list/1, float_to_int_color/2, build_ppm_data/1, save_ppm_data/2, load_ppm_data/1, to_integer/1]).
-include("records.hrl").

-define(MAX_VALUE, 255).

%% @doc Ready to save ppm data from a matrix of pixels.
%% @spec build_ppm_data(list(list(color()))) -> ppm_data()
build_ppm_data(Pixels) ->
    #ppm_data{width=width(Pixels),
	      height=height(Pixels),
	      max_color=?MAX_COLOR,
	      pixels=lists:map(fun(Color) -> float_to_int_color(?MAX_COLOR, Color) end,
			  matrix_to_list(Pixels))}.

%% @doc Int coded rgb color with 
float_to_int_color(MaxColor, #color{r=R,g=G,b=B}) ->
    #color{r=lists:min([trunc(R*MaxColor), MaxColor]),
	   g=lists:min([trunc(G*MaxColor), MaxColor]),
	   b=lists:min([trunc(B*MaxColor), MaxColor])}.

save(Pixels,Filename) ->
    save_ppm_data(build_ppm_data(Pixels),Filename).

save_ppm_data(#ppm_data{width=Width,height=Height,max_color=MaxColor,pixels=Pixels}, Filename) ->
    case file:open(Filename, write) of
	{ok, IoDevice} ->
	    io:format(IoDevice, "P3~n", []),
	    io:format(IoDevice, "~p ~p~n", [Width, Height]),
	    io:format(IoDevice, "~p~n", [MaxColor]),
	    lists:foreach(
	      fun(#color{r=R,g=G,b=B}) -> io:format(IoDevice, "~p ~p ~p ", [R,G,B]) end,
	      Pixels),
	    file:close(IoDevice),
	    ok;
	Error -> Error
    end.

load_ppm_data(Filename)->
    case file:read_file(Filename) of
	{ok, Bin} ->
	    read_ppm_data(binary_to_list(Bin));
	Error -> Error
    end.

read_ppm_data("P3" ++ Body) ->
    Tokens = string:tokens(Body, " \n"),
    IntTokens = lists:map(fun(Token) -> to_integer(Token) end, Tokens),
    read_ppm_tokens(IntTokens).

to_integer(String) ->
    {Result, _Rest} = string:to_integer(String),
    Result.    

read_ppm_tokens([Width,Height,MaxColor|Colors]) ->
    #ppm_data{width=Width,
	      height=Height,
	      max_color=MaxColor,
	      pixels=read_ppm_colors(Colors)}.

read_ppm_colors([]) -> [];
read_ppm_colors([R,G,B|Others]) -> [#color{r=R,g=G,b=B} | read_ppm_colors(Others)].

width([]) -> 0;
width([FirstRow|_OtherRows]) ->
    length(FirstRow).

height(Matrix) ->
    length(Matrix).

matrix_to_list([])->
    [];
matrix_to_list([[]|OtherRows]) ->
    matrix_to_list(OtherRows);
matrix_to_list([[FirstItem|OtherItems]|OtherRows]) ->
    [FirstItem|matrix_to_list([OtherItems|OtherRows])].
