-module(make).
-export([doc/0, local_test/0, start_server/0, kill/1, start_test_clients/1]).

%% eunit not yet installed ...
%%-include_lib("eunit/include/eunit.hrl").

doc() ->
    edoc:files(["gui.erl", "client.erl", "room.erl", "chat.erl", "utils.erl", "bict.erl"]).

local_test() ->
    chat:start(),
    ok = client:start("AC", "Parents"),
    ok = client:start("Phil", "Parents"),
    ok = client:start("Noah", "Enfants"),
    ok = client:start("Petite soeur", "Enfants").

start_server() ->
    chat:start().

start_test_clients([ServerNode]) ->
    client:start_remote("AC", ServerNode, "Parents"),
    client:start_remote("Phil", ServerNode, "Parents"),
    client:start_remote("Noah", ServerNode, "Enfants"),
    client:start_remote("Petite soeur", ServerNode, "Enfants").
    
kill([RemoteNode]) ->
    catch(rpc:call(RemoteNode, init, stop, [])).

