-module(achievement).
-export([
    name/1,
    description/1,
    icon/1,
    eq/2,
    to_map/1
    ]).

-type achievement() :: { Name::binary(), Description::binary(), Icon::binary()}.
-export_type([achievement/0]).

-spec name(achievement()) -> binary().
name({Name, _, _}) ->
    Name.

-spec description(achievement()) -> binary().
description({_, Description, _}) ->
    Description.

-spec icon(achievement()) -> binary().
icon({_, _, Icon}) ->
    Icon.

-spec eq(achievement(), achievement()) -> boolean().
eq({LhName, _, _}, {RhName, _, _}) ->
    LhName =:= RhName.

-spec to_map(achievement()) -> map().
to_map({Name, Description, Icon}) ->
    #{ <<"name">> => Name, <<"description">> => Description, <<"icon">> => Icon}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accessors_test_() ->
    Achi = {<<"achi">>, <<"desc">>, <<"noicon">>},
    [
        {"name", ?_assertEqual(<<"achi">>, achievement:name(Achi))},
        {"description", ?_assertEqual(<<"desc">>, achievement:description(Achi))},
        {"icon", ?_assertEqual(<<"noicon">>, achievement:icon(Achi))}
    ].

-endif.