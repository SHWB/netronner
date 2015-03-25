-module(netronner_SUITE).
-compile(export_all).

all() ->
    [can_start,
     {group, killswitch}].

groups() ->
    [{killswitch,
        [parallel],
        [killswitch_503,
         killswitch_503_retry_after_header]}].

init_per_test(_, Config) ->
    Config.
end_per_test(_, Config) ->
    Config.

init_per_group(_, Config) ->
    ok = start_test_netronner(),
    killswitch:set_readonly(),
    Config.
end_per_group(_, Config) ->
    ok = application:stop(netronner),
    Config.

%% helpers

start_test_netronner() ->
    ok = application:set_env(netronner, protocol, http),
    {ok, _Apps} = application:ensure_all_started(netronner),
    ok.

post(Path) ->
    {ok, Response} = httpc:request(post,
                  {"http://localhost:8080" ++ Path, [], "x-www-form-urlencoded", "achievement=Kan Not"},
                  [{timeout, 500},
                   {autoredirect, false}],
                  []),
    Response.

%% tests

can_start(_Config) ->
    ok = start_test_netronner().

killswitch_503(_Config) ->
    {{"HTTP/1.1", 503, _}, _, _} = post("/players/dummyplayer/achievements").

killswitch_503_retry_after_header(_Config) ->
    {{"HTTP/1.1", 503, _}, Headers, _} = post("/players/dummyplayer/achievements"),
    true = proplists:is_defined("retry-after", Headers).

