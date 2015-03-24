-module(netronner_SUITE).
-export([all/0]).
-export([init_per_test/2]).
-export([end_per_test/2]).
-export([can_start/1]).

all() ->
    [can_start].

init_per_test(_, Config) ->
    Config.
end_per_test(_, Config) ->
    Config.

can_start(Config) ->
    application:set_env(netronner, protocol, http),
    {ok, _Apps} = application:ensure_all_started(netronner).