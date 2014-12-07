-module(achievements_handler).
-export([
    init/3,
    rest_init/2,
    content_types_provided/2,
    get_achievements_json/2
    ]).

init(_Transport, Req, []) ->
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    {upgrade, protocol, cowboy_rest, Req2, []}.

rest_init(Req, State) ->
    {ok, Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, get_achievements_json}], Req, State}.

get_achievements_json(Req, State) ->
    Achievements = achievements:list(),
    {encode_json(Achievements), Req, State}.

%% private

-spec encode_json([achievement:achievement()]) -> binary().
encode_json(Achievements) when is_list(Achievements) ->
    AsMaps = lists:map(fun achievement:to_map/1, Achievements),
    jiffy:encode(AsMaps).
