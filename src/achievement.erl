-module(achievement).
-export([
    name/1,
    description/1,
    flavour_text/1,
    implied_achievements/1,
    eq/2,
    to_map/1
    ]).

-type achievement() :: {
    Name::binary(),
    Description::binary(),
    FlavourText::binary(),
    Implies::list(Name::binary())
    }.
-export_type([achievement/0]).

-spec name(achievement()) -> binary().
name({Name, _, _, _}) ->
    Name.

-spec description(achievement()) -> binary().
description({_, Description, _, _}) ->
    Description.

-spec flavour_text(achievement()) -> binary().
flavour_text({_, _, FlavourText, _}) ->
    FlavourText.

-spec implied_achievements(achievement()) -> list(binary()).
implied_achievements({_, _, _, Implied}) ->
    Implied.

-spec eq(achievement(), achievement()) -> boolean().
eq({LhName, _, _, _}, {RhName, _, _, _}) ->
    LhName =:= RhName.

-spec to_map(achievement()) -> map().
to_map({Name, Description, FlavourText, _}) ->
    #{ <<"name">> => Name, <<"description">> => Description, <<"flavour_text">> => FlavourText}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accessors_test_() ->
    Achi = {<<"achi">>, <<"desc">>, <<"flav">>, [<<"other">>]},
    [
        {"name", ?_assertEqual(<<"achi">>, achievement:name(Achi))},
        {"description", ?_assertEqual(<<"desc">>, achievement:description(Achi))},
        {"flav", ?_assertEqual(<<"flav">>, achievement:flavour_text(Achi))},
        {"implied", ?_assertEqual([<<"other">>], achievement:implied_achievements(Achi))}
    ].

equals_when_name_matches_test() ->
    L = {<<"match">>, <<"a">>, <<"a">>, []},
    R = {<<"match">>, <<"b">>, <<"b">>, []},
    ?assert(eq(L, R)).

-endif.