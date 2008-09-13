%% @doc Bidirectional dictionary handling functions.
-module(bict).
-export([new/0, length/1, keys/1, values/1, store/3, find_key/2, find_value/2, fetch_value/2, fetch_key/2, erase_key/2, erase_value/2]).

%% @type bict(). A bidirectional dictionary, keys to values and values to keys.

%% @doc Creates a new bi-dictionary.
%% @spec new() -> bict()
new() ->
    { dict:new(), dict:new() }.

%% @doc Number of items in the bict.
%% @spec length(bict()) -> number()
length({Keys_2_values, _Values_2_keys}) ->
    dico_length(Keys_2_values).

dico_length(Dico) ->
    dict:fold(fun(_Key, _Value, Acc) -> Acc + 1 end, 0, Dico).

%% @doc List of the keys in the bict()
%% @spec keys(bict()) -> list()
keys({_Keys_2_values, Values_2_keys}) ->
    dico_values(Values_2_keys).

%% @doc List of the values in the bict()
%% @spec values(bict()) -> list()
values({Keys_2_values, _Values_2_keys}) ->
    dico_values(Keys_2_values).

dico_values(Dictionary) ->
    dict:fold(fun(_Key, Value, Acc) -> [ Value | Acc] end, [], Dictionary).

%% @doc Stores a new Key Value pair
%% @spec store(Key,Value,bict())->bict()
store(Key, Value, {Keys_2_values, Values_2_keys}) ->
    {dict:store(Key,Value,Keys_2_values), dict:store(Value,Key, Values_2_keys)}.

%% @doc Searches the value associated with a key in the bict.
%% @spec find_value(Key, bict()) -> {ok, Value} | error
find_value(Key, {Keys_2_values, _Values_2_keys}) ->
    dict:find(Key, Keys_2_values).

%% @doc Searches the key associated with a value in the bict.
%% @spec find_key(Value, bict()) -> {ok, Key} | error
find_key(Value, {_Keys_2_values, Values_2_keys}) ->
    dict:find(Value, Values_2_keys).

%% @doc Searches the value associated with a key in the bict.
%% @spec fetch_value(Key, bict()) -> Value
fetch_value(Key, {Keys_2_values, _Values_2_keys}) ->
    dict:fetch(Key, Keys_2_values).

%% @doc Searches the key associated with a value in the bict.
%% @spec fetch_key(Value, bict()) -> Key
fetch_key(Value, {_Keys_2_values, Values_2_keys}) ->
    dict:fetch(Value, Values_2_keys).

%% @doc Removes a value (and its associated key) from a bict.
%% @spec erase_value(Value, bict()) -> bict()
erase_value(Value, {Keys_2_values, Values_2_keys} = Bict) ->
    case find_key(Value, Bict) of
	{ ok, Key } ->
	    { dict:erase(Key, Keys_2_values), dict:erase(Value, Values_2_keys)};
	error ->
	    Bict
    end.

%% @doc Removes a key (and its associated value) from a bict.
%% @spec erase_key(Key, bict()) -> bict()
erase_key(Key, {Keys_2_values, Values_2_keys} = Bict) ->
    case find_value(Key, Bict) of
	{ ok, Value } ->
	    { dict:erase(Key, Keys_2_values), dict:erase(Value, Values_2_keys)};
	error ->
	    Bict
    end.
