-module(achievement_definitions).
-export([
    all/0
    ]).

-spec all() -> Achievements::list(achievement:achievement()).
all() ->
    [
    {<<"8th Wonder">>,
     <<"Win a bonus hand with at least 8 Honba (bonus counters) are in play">>,
     <<"">>, [], undefined},
    {<<"A Walk in the Woods">>,<<"Score a Bamboo Full-Flush">>,<<"">>, [], undefined},
    {<<"Adoptive Parent">>,<<"Score a Thirteen Orphans">>,<<"">>, [], undefined},
    {<<"Against a Common Enemy">>,<<"Take part in a Double Ron">>,<<"">>, [], undefined},
    {<<"All Green!">>,<<"Score a Green Hand">>,<<"">>, [], undefined},
    {<<"Blitzkrieg">>,<<"Score Double Riichi and One-Shot in the same hand">>,
     <<"">>, [<<"O Hai, I'm in Tenpai!">>, <<"Ludicrous Speed">>], undefined},
    {<<"Bowling Alley">>,<<"Score a Circles Full-Flush">>,<<"">>, [], undefined},
    {<<"Can't Hold all These Doras (Rank 1)">>,
     <<"Win a hand with 6 or more Doras (Red Doras and Hidden Doras count too)">>,
     <<"">>, [], {<<"Can't Hold all These Doras">>, 1}},
    {<<"Can't Hold all These Doras (Rank 2)">>,
     <<"Win a hand with 9 or more Doras (Red Doras and Hidden Doras count too)">>,
     <<"">>, [<<"Can't Hold all These Doras (Rank 1)">>], {<<"Can't Hold all These Doras">>, 2}},
    {<<"Can't Hold all These Doras (Rank 3)">>,
     <<"Win a hand with 12 or more Doras (Red Doras and Hidden Doras count too)">>,
     <<"">>, [<<"Can't Hold all These Doras (Rank 2)">>], {<<"Can't Hold all These Doras">>, 3}},
    {<<"Crash Course">>,
     <<"Force an abortive draw by declaring the fourth Riichi">>,<<"">>, [], undefined},
    {<<"D I S C O N N E C T E D">>,
     <<"Open an initial hand with no pairs, triples, quadruples nor two contiguous tiles in the same suit (brotip: take a screenshot of the hand to claim the achivement at the end of hand and keep on playing)">>,
     <<"">>, [], undefined},
    {<<"Deja Vu">>,
     <<"Call twice the same tile to complete two identical Chows in a single game">>,
     <<"A deja vu is usually a glitch in the Matrix. It happens when they chi something.">>, [], undefined},
    {<<"Delusions of Victory">>,
     <<"Declare a false Ron and pay a chombo penalty for it">>,<<"LOL JK!!11!!one!">>, [], undefined},
    {<<"Die Hard">>,
     <<"Terminate a game with less than 1000 points (before applying winner bonuses)">>,
     <<"It's just a flesh wound.">>, [], undefined},
    {<<"Dragon Trainer">>,<<"Score a Little Three Dragons">>,<<"">>, [], undefined},
    {<<"Fanpaistic!">>,
     <<"Call a Fanpai Pon during the first uninterrupted go-round.">>,
     <<"">>, [], undefined},
    {<<"Four o' Four">>,<<"Force an abortive draw by calling the fourth Kan">>,
     <<"">>, [], undefined},
    {<<"Grenadier">>,
     <<"Reduce two opponents' points below zero with a single Tsumo">>,
     <<"">>, [<<"Shot on the Crowd">>], undefined},
    {<<"Han Solo">>,
     <<"Win waiting for a 1-out Character tile (to determine the outs only tiles visible to the player counts. This means open melds, Dora indicators, the river and own's hand)">>,
     <<"Great shot, kid, that was one in a million!">>, [<<"Hell Wait">>], undefined},
    {<<"Hell Wait">>,
     <<"Win a hand with a 1-out wait (to determine the outs only tiles visible to the player counts. This means open melds, Dora indicators, the river and own's hand)">>,
     <<"">>, [], undefined},
    {<<"I Have an Encoding Problem!">>,
     <<"Win with a hand composed entirely of Pairs/Pons/Kans of either Characters tiles or White Dragons, then declare BEGINNING OF COSMOS!">>,
     <<"">>, [], undefined},
    {<<"I Run an Orphanage">>,
     <<"Score a Thirteen Orphans on a thirteen-sided wait">>,<<"">>, [<<"Adoptive Parent">>], undefined},
    {<<"I'll Do It Myself!">>,
     <<"Score a Riichi while being Furiten (Temporarily Furiten counts, too)">>,
     <<"">>, [<<"Thanks for Nothing">>], undefined},
    {<<"I'm On Fire!">>,<<"Stay east for 5 consecutive hands">>,<<"">>, [], undefined},
    {<<"In the Eye of the Storm">>,<<"Score a Big Four Winds">>,<<"">>, [], undefined},
    {<<"Invisible">>,
     <<"Terminate a Hanchan (East-South match) without any of your tiles being called">>,
     <<"">>, [<<"Lack of Presence">>], undefined},
    {<<"It Is (Not) Alone">>,
     <<"Win on a pair wait on the last closed tile in your hand">>,
     <<"">>, [], undefined},
    {<<"It Looks Easier From the Outside">>,
     <<"Score a Pinfu">>,
     <<"">>, [], undefined},
    {<<"Kan I Win?">>,<<"Score an After a Kong">>,<<"">>, [], undefined},
    {<<"Kan Not">>,<<"Discard four times the same tile in a single game">>,
     <<"">>, [], undefined},
    {<<"Keep it Simple">>,<<"Score a Tanyao">>,<<"">>, [], undefined},
    {<<"King of the Mountain">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of +140 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 10)">>], {<<"To the Top">>, 11}},
    {<<"Lack of Presence">>,
     <<"Terminate either a Tonpuusen (East-Only match) or the East hand of an Hanchan (East-South match) without any of your tiles being called">>,
     <<"">>, [], undefined},
    {<<"Last Airbender">>,<<"Score a Little Four Winds">>,<<"">>, [], undefined},
    {<<"Literate">>,<<"Score a Characters Full-Flush">>,<<"">>, [], undefined},
    {<<"Little Mermaid">>,<<"Score a Bottom of the Sea">>,<<"">>, [], undefined},
    {<<"Ludicrous Speed">>,<<"Score a One-Shot">>,<<"">>, [], undefined},
    {<<"Mother of Dragons">>,<<"Score a Big Three Dragons">>,<<"">>, [], undefined},
    {<<"Nine Etched Fails">>,
     <<"Force an abortive draw by having 9 or more different orphans on the opening hand">>,
     <<"">>, [], undefined},
    {<<"No Pain, No Gain">>,
     <<"Terminate a full game (either East-South or East-only) with a result of +-0 after awarding the winner bonuses">>,
     <<"">>, [], undefined},
    {<<"Nuke From Orbit">>,
     <<"Reduce three opponents' points below zero with a single tsumo">>,
     <<"Nuclear launch detected.">>, [<<"Grenadier">>], undefined},
    {<<"O Hai, I'm in Tenpai!">>,<<"Score a Double Riichi">>,<<"">>, [], undefined},
    {<<"Oath of Allegiance">>,<<"Participate in a Triple Ron">>,<<"">>, [], undefined},
    {<<"One Shot One Kill">>,
     <<"Reduce an opponent that had 25000 or more points at the beginning of the match to zero points with a direct hit">>,
     <<"">>, [<<"Player Killer">>], undefined},
    {<<"Out of Reach">>,
     <<"Call a Ron on the Riichi declaration tile of another player">>,
     <<"">>, [], undefined},
    {<<"Pick a Seaweed From the Bottom of the Ocean">>,
     <<"Score Bottom of the Sea and After a Kan in the same game">>,
     <<"">>, [<<"Kan I Win?">>, <<"Little Mermaid">>], undefined},
    {<<"Ping Pon">>,
     <<"Have two players call four consecutive Pons from each other (awarded to both)">>,
     <<"">>, [], undefined},
    {<<"Pinterest">>,
     <<"Call 4 or more Circle tiles from your opponents in a single game">>,
     <<"">>, [], undefined},
    {<<"Player Killer">>,
     <<"Reduce an opponent's points below zero with a direct hit">>,
     <<"">>, [], undefined},
     {<<"RageQuit">>,
     <<"Do it">>,
     <<"Fuck this shit, I'm out of here!">>, [], undefined},
    {<<"Sanpei">>,<<"Score a Bottom of the River">>,<<"">>, [], undefined},
    {<<"Save the Children From The Bad Guys">>,
     <<"Score a Thirteen Orphans robbing a closed Kan">>,<<"">>, [<<"Adoptive Parent">>], undefined},
    {<<"Save Tsumonday!">>,<<"Play your first game">>,<<"">>, [], undefined},
    {<<"Seventh Son of a Seventh Tile">>,<<"Score a Seven Pairs">>,
     <<"">>, [], undefined},
    {<<"Shot on the Crowd">>,
     <<"Reduce an opponent's points below zero with a Tsumo">>,<<"">>, [<<"Player Killer">>], undefined},
    {<<"Siberian Express">>,
     <<"Win with a Circle Nine Gates. You must shout SIBERIAN EXPRESS! while striking an epic pose">>,
     <<"">>, [], undefined},
    {<<"Strong">>,<<"Win a game with 1 pung of ",226,145,168," in your hand (open or closed)">>,
     <<"I'm a strong fairy!">>, [], undefined},
    {<<"Stronger">>,
     <<"Win a game with 2 pungs of ",226,145,168," in your hand (open or closed)">>,
     <<"I'm a stronger fairy!">>, [<<"Strong">>], undefined},
    {<<"Strongest">>,
     <<"Win a game with 3 pungs of ",226,145,168," in your hand (open or closed)">>,
     <<"I'm the strongest fairy in the world!">>, [<<"Stronger">>, <<"Strong">>], undefined},
    {<<"Strongester">>,
     <<"Win a game with 1 kong of ",226,145,168," in your hand (open or closed)">>,
     <<"I'm the strongester fairy in the world!">>, [<<"Strong">>], undefined},
    {<<"Strongestest">>,
     <<"Win a game with 2 kongs of ",226,145,168," in your hand (open or closed)">>,
     <<"I'm the strongestest fairy in the world!">>, [<<"Strongester">>, <<"Stronger">>, <<"Strong">>], undefined},
    {<<"Strongestestest">>,
     <<"Win a game with 3 kongs of ",226,145,168," in your hand (open or closed)">>,
     <<"I'm the strongestestest fairy in the world!">>, [<<"Strongestest">>, <<"Strongester">>, <<"Strongest">>, <<"Stronger">>, <<"Strong">>], undefined},
    {<<"Superman">>,
     <<"Terminate a full game (either East-South or East-Only) without losing a single point (losing Riichi bets counts, too)">>,
     <<"">>, [], undefined},
    {<<">tfw noten">>,
     <<"Be the only player not in tenpai at the end of draw round">>,
     <<"East is a fag">> [], undefined},
    {<<"Thanks for Nothing">>,<<"Score a Fully Concealed Tsumo">>,<<"">>, [], undefined},
    {<<"They See Me Ronning, They Hatin'">>,
     <<"Declare three consecutive Rons from three different players">>,
     <<"Haters gonna hate.">>, [], undefined},
    {<<"Tiles Are Red, Violets Are Blue...">>,
     <<"Win with all three Red Doras and all four Red Dragons">>,<<"">>, [], undefined},
    {<<"To the Top (Rank 1)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +30 points (after awarding winner bonuses)">>,
     <<"">>, [], {<<"To the Top">>, 1}},
    {<<"To the Top (Rank 2)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +40 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 1)">>], {<<"To the Top">>, 2}},
    {<<"To the Top (Rank 3)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +50 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 2)">>], {<<"To the Top">>, 3}},
    {<<"To the Top (Rank 4)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +60 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 3)">>], {<<"To the Top">>, 4}},
    {<<"To the Top (Rank 5)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +70 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 4)">>], {<<"To the Top">>, 5}},
    {<<"To the Top (Rank 6)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +80 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 5)">>], {<<"To the Top">>, 6}},
    {<<"To the Top (Rank 7)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +90 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 6)">>], {<<"To the Top">>, 7}},
    {<<"To the Top (Rank 8)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +100 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 7)">>], {<<"To the Top">>, 8}},
    {<<"To the Top (Rank 9)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +110 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 8)">>], {<<"To the Top">>, 9}},
    {<<"To the Top (Rank 10)">>,
     <<"Terminate a full game (either East-South or East-Only) with a score of at least +120 points (after awarding winner bonuses)">>,
     <<"">>, [<<"To the Top (Rank 9)">>], {<<"To the Top">>, 10}},
    {<<"Unlucky Fourth Object">>,
     <<"Force an abortive draw by discarding the fourth same wind in the first round">>,
     <<"">>, [], undefined},
    {<<"Unriichiable">>,
     <<"Declare a Riichi with a 0-out wait (to determine the outs only tiles visible to the player counts. This means open melds, Dora indicators, the river and own's hand)">>,
     <<"">>, [], undefined},
    {<<"Vulgar Display of Power">>,
     <<"Take precedence of a Chi/Pon declaration with a Pon/Ron declaration">>,
     <<"">>, [], undefined}
    ].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

all_encodable_test_() ->
    [ {achievement:name(A), ?_test(jiffy:encode(achievement:to_map(A))) } || A <- all()].

-endif.