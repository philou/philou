%% @doc Various polynoms of X functions. Polynoms are represented as a list of coefficients ... the head is degree 0.
-module(polynoms).
-compile(export_all).
-export([add/2,add/1,scale/2,mult_by_x/1,mult/2,pow/2,reduce/1]).

%% @doc Adds two polynoms.
%% @spec add(pol(),pol()) -> pol()
add([],PolB) ->
    PolB;
add(PolA,[]) ->
    PolA;
add([CA|PolA],[CB|PolB]) ->
    [CA+CB|add(PolA,PolB)].

%% @doc Adds many polynoms.
%% @spec add([pol()]) -> pol()
add(Pols) ->
    lists:foldr(fun add/2,[],Pols).

%% @doc Multiplies a polynom by a constant.
%% @spec scale(int(), pol()) -> pol()
scale(K,Pol) ->
    lists:map(fun(C) -> K*C end,Pol).

%% @doc Multiplies a polynom by X.
%% @spec mult_by_x(pol()) -> pol()
mult_by_x(Pol)->
    [0|Pol].

%% @doc Multiplies two polynoms wit each other.
%% @spec mult(pol(),pol()) -> pol()
mult([],_PolB)->
    [];
mult([C|PolA],PolB) ->
    add(scale(C,PolB),mult_by_x(mult(PolA,PolB))).

%% @doc Multiplies a polynom with itself N times.
%% @spec pow(pol(),int()) -> pol()
pow(_Pol,0) ->
    [1];
pow(Pol,1) ->
    Pol;
pow(Pol,N) ->
    mult(Pol,pow(Pol,N-1)).

%% @doc List of the roots of a polynom.
%% @spec roots(pol()) -> [float()]
roots([B,A]) when (A /= 0) ->
    [-B/A];
roots([C,B,A]) when (A /= 0) ->
    case B*B -4*A*C of
	Det when Det < 0 -> [];
	Det when Det == 0 -> [-B/(2*A)];
	Det when Det > 0 -> [(-B - math:sqrt(Det))/(2*A),(-B + math:sqrt(Det))/(2*A)]
    end.

%% @doc Equivalent polynom with its first coefficient not null.
%% @spec reduce(pol()) -> pol()
reduce(Pol) ->
    lists:foldr(fun acc_reduce/2,[],Pol).
acc_reduce(C,[]) when C == 0 ->
    [];
acc_reduce(C, Res) ->
    [C|Res].

%% @doc Evaluates a polynom with a given X value.
%% @spec eval(pol(),float()) -> float()
eval([],_X) ->
    0;
eval([C|Pol],X) ->
    C + X*eval(Pol,X).
