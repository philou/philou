%% @doc Vectors and coordonates manipulating functions.
-module(vectors).
-export([x/1,y/1,z/1,add/2,scale/2,from_coords/2,move/2,norm/1,normalize/1,dot/2]).
-include("records.hrl").

%% @doc X coordonate.
%% @spec x(vector() | coord()) -> float()
x({_type,X,_Y,_Z}) -> X.
%% @doc Y coordonate.
%% @spec y(vector() | coord()) -> float()
y({_type,_X,Y,_Z}) -> Y.
%% @doc Z coordonate.
%% @spec z(vector() | coord()) -> float()
z({_type,_X,_Y,Z}) -> Z.

%% @doc Adds two vectors.
%% @spec add(vector(),vector()) -> vector()
add(#vector{x=X1,y=Y1,z=Z1},#vector{x=X2,y=Y2,z=Z2}) ->
    #vector{x=X1+X2,y=Y1+Y2,z=Z1+Z2}.

%% @doc Scales a vector.
%% @spec scale(float(),vector()) -> vector()
scale(K,#vector{x=X,y=Y,z=Z}) ->
    #vector{x=K*X,y=K*Y,z=K*Z}.

%% @doc Creates a vector from two coordonates.
%% @spec from_coords(coord(),coord()) -> vector()
from_coords(#coord{x=X1,y=Y1,z=Z1},#coord{x=X2,y=Y2,z=Z2}) ->
    #vector{x=X2-X1,y=Y2-Y1,z=Z2-Z1}.

%% @doc Image of a point by a translation.
%% @spec move(coord(),vector()) -> coord()
move(#coord{x=X1,y=Y1,z=Z1},#vector{x=X2,y=Y2,z=Z2}) ->
    #coord{x=X1+X2,y=Y1+Y2,z=Z1+Z2}.

%% @doc Length of a vector.
%% @spec norm(vector()) -> float()
norm(V) ->
    math:sqrt(dot(V,V)).

%% @doc Vector with the same direction, but with length 1.
%% @spec normalize(vector()) -> vector()
normalize(V) ->
    case norm(V) of
	0.0 -> ?NULL_VECTOR;
	N -> scale(1/N,V)
    end.

%% @doc Dot product of two vectors.
%% @spec dot(vector(),vector()) -> float()
dot(#vector{x=X1,y=Y1,z=Z1},#vector{x=X2,y=Y2,z=Z2})->
    X1*X2 + Y1*Y2 + Z1*Z2.
