-module(netronner_authorization_infector).

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_info/2, handle_call/2, code_change/3, terminate/2]).
-export([initialize_infection_list/0]).

init([]) ->
    {ok, []}.

handle_event({achievement_award, Player, _Achievement}, State) ->
    {PlayerId, _,  _, _} = Player,
    ok = google_viral_authorization:infect(PlayerId),
    {ok, State}.

handle_info(_Msg, State) ->
    {ok , State}.

handle_call(_Req, State) ->
    {ok, {error, badrequest}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

initialize_infection_list() ->
    scan_timeline_page(latest).

scan_timeline_page(none) ->
    ok;
scan_timeline_page(Page) ->
    {_Page, Prev, Events} = timeline:page(Page),
    [ google_viral_authorization:infect(GoogleId) || {<<"achievement_award">>, _ , #{<<"player">> := #{<<"id">> := GoogleId}}} <- Events],
    scan_timeline_page(Prev).