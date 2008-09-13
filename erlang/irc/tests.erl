-module(tests).
-include_lib("eunit/include/eunit.hrl").


% new
% find_key, find_value
% fetch_key, fetch_value
% store
% erase
% length
% values
% keys
% map
% ? foreach ???

femme_AC_bict() ->
    bict:store("femme","AC", bict:new()).

two_items_bict() ->
    bict:store("homme","Phil", femme_AC_bict()).

bict_test_() ->
    [?_assertEqual(0, bict:length(bict:new())),
     ?_assertEqual(1, bict:length(femme_AC_bict())),
     ?_assertEqual(2, bict:length(two_items_bict())),
     ?_assertEqual([], bict:values(bict:new())),
     ?_assertEqual(["AC"], bict:values(femme_AC_bict())),
     ?_assertEqual([], bict:keys(bict:new())),
     ?_assertEqual(["femme"], bict:keys(femme_AC_bict())),
     ?_assertEqual({ok, "AC"}, bict:find_value("femme", femme_AC_bict())),
     ?_assertEqual(error, bict:find_value("femme", bict:new())),
     ?_assertEqual({ok, "femme"}, bict:find_key("AC", femme_AC_bict())),
     ?_assertEqual(error, bict:find_key("AC", bict:new())),
     ?_assertEqual("AC", bict:fetch_value("femme", femme_AC_bict())),
     ?_assertError(function_clause, bict:fetch_value("femme", bict:new())),
     ?_assertEqual("femme", bict:fetch_key("AC", femme_AC_bict())),
     ?_assertError(function_clause, bict:fetch_key("AC", bict:new())),
     ?_assertEqual(0, bict:length(bict:erase_value("AC", bict:new()))),
     ?_assertEqual(0, bict:length(bict:erase_value("AC", femme_AC_bict()))),
     ?_assertEqual(0, bict:length(bict:erase_key("femme", bict:new()))),
     ?_assertEqual(0, bict:length(bict:erase_key("femme", femme_AC_bict())))
     ].

