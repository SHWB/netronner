-module(player_handler).
-export([
    init/3,
    rest_init/2,
    resource_exists/2,
    content_types_provided/2,
    get_player_json/2
    ]).

init(_Transport, Req, [Repository]) ->
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    {upgrade, protocol, cowboy_rest, Req2, Repository}.

rest_init(Req, Repository) ->
    {ok, Req, Repository}.    

resource_exists(Req, Repository) ->
    {PlayerId, _} = cowboy_req:binding(player_id, Req),
    Exists = case players:load(PlayerId, Repository) of
        {ok, _} -> true;
        notfound -> false
    end,
    {Exists, Req, Repository}.


content_types_provided(Req, Repository) ->
    {[{<<"application/json">>, get_player_json}], Req, Repository}.

get_player_json(Req, Repository) ->
    {PlayerId, _} = cowboy_req:binding(player_id, Req),
    {ok, Player} = players:load(PlayerId, Repository),
    {encode_json(Player), Req, Repository}.


%% private
-spec encode_json(player:player()) -> binary().
encode_json(Player) ->
    AsMap = #{
            <<"id">> => player:id(Player),
            <<"name">> => player:name(Player),
            <<"imageUrl">> => player:image_url(Player),
            <<"achievements">> => lists:map(fun achievement:to_map/1, player:achievements(Player))
        },
    jiffy:encode(AsMap).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    AchievementInSeries = lists:keyfind(<<"To the Top (Rank 1)">>, 1, achievement_definitions:all()),
    Player = player:with_achievement(AchievementInSeries, player:new(<<"123">>, <<"someone">>, <<"">>)),
    #{ <<"achievements">> := [EncodedAchievement]} = jiffy:decode(encode_json(Player), [return_maps]),
    [
        {"player achievement has series", ?_assert(maps:is_key(<<"series">>, EncodedAchievement))},
        {"player achievement has rank", ?_assert(maps:is_key(<<"rank">>, EncodedAchievement))}
    ].

-endif.