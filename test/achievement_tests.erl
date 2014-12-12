-module(achievement_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

accessors_test_() ->
    Achi = achievement:new(<<"achi">>, <<"desc">>, <<"noicon">>),
    [
        ?_assertEqual(<<"achi">>, achievement:name(Achi)),
        ?_assertEqual(<<"desc">>, achievement:description(Achi)),
        ?_assertEqual(<<"noicon">>, achievement:icon(Achi))
    ].