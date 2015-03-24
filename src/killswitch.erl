-module(killswitch).

%% API.
-export([is_readonly/0]).
-export([is_readwrite/0]).
-export([set_readonly/0]).
-export([set_readwrite/0]).

%% possible evolutions: ets table with per-feature switch

is_readonly() ->
    ro =:= get_state().

is_readwrite() ->
    rw =:= get_state().

get_state() ->
    application:get_env(killswitch, state, rw).

set_readonly() ->
    application:set_env(killswitch, state, ro).

set_readwrite() ->
    application:set_env(killswitch, state, rw).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

killswitch_test_() -> [
    {"default is readwrite", 
        ?_assert(killswitch:is_readwrite()) },
    {"can set readonly",
        fun() -> 
            killswitch:set_readonly(),
            ?assert(killswitch:is_readonly())
        end },
    {"can set readonly then reset to readwrite",
        fun() ->
            killswitch:set_readonly(),
            killswitch:set_readwrite(),
            ?assert(killswitch:is_readwrite())
        end }
].

-endif.