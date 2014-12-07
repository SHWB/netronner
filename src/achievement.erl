-module(achievement).
-export([
    new/3,
    name/1,
    description/1,
    icon/1,
    eq/2,
    from_map/1,
    to_map/1
    ]).

-type achievement() :: { Name::binary(), Description::binary(), Icon::binary()}.
-export_type([achievement/0]).

-spec new(binary(), binary(), binary()) -> achievement().
new(Name, Description, Icon) ->
    {Name, Description, Icon}.

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

-spec from_map(map()) -> achievement().
from_map(#{ <<"name">> := Name, <<"description">> := Description, <<"icon">> := Icon}) when is_binary(Name), is_binary(Description), is_binary(Icon) ->
    {Name, Description, Icon}.

-spec to_map(achievement()) -> map().
to_map({Name, Description, Icon}) ->
    #{ <<"name">> => Name, <<"description">> => Description, <<"icon">> => Icon}.
