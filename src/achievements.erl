-module(achievements).
-behaviour(gen_server).
-export([
    start_link/0
    ]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).
-export([
    list/0,
    load/1
    ]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec list() -> [achievement:achievement()].
list() ->
    gen_server:call(?MODULE, list).

-spec load(AchievementName::binary()) -> {ok, achievement:achievement()} | notfound.
load(AchievementName) ->
    gen_server:call(?MODULE, {load, AchievementName}).

%% gen_server.

init([]) ->
    TableHandle = ets_from_json(),
    {ok, TableHandle}.

handle_call(list, _From, TableHandle) ->
    {reply, list(TableHandle), TableHandle};
handle_call({load, AchievementName}, _From, TableHandle) ->
    {reply, load(AchievementName, TableHandle), TableHandle};
handle_call(_Request, _From, TableHandle) ->
    {reply, ignored, TableHandle}.

handle_cast(_Msg, TableHandle) ->
    {noreply, TableHandle}.

handle_info(_Info, TableHandle) ->
    {noreply, TableHandle}.

terminate(_Reason, _TableHandle) ->
    ok.

code_change(_OldVsn, TableHandle, _Extra) ->
    {ok, TableHandle}.

%% internals.

-spec list(ets:tab()) -> [achievement:achievement()].
list(TableHandle) ->
    ets:foldr(fun(El, Memo) -> [El|Memo] end, [], TableHandle).

-spec load(AchievementName::binary(), TableHandle::ets:tab()) -> {ok, achievement:achievement()} | notfound.
load(AchievementName, TableHandle) ->
    case ets:lookup(TableHandle, AchievementName) of
        [Achievement] -> {ok, Achievement};
        [] -> notfound
    end.

ets_from_json() ->
    TableHandle = ets:new(?MODULE, [ordered_set]),
    ets:insert(TableHandle, jsonfile_to_achievements()),
    TableHandle.

jsonfile_to_achievements() ->
    Filename = filename:join(code:priv_dir(netronner),"achievements.json"),
    {ok, Content} = file:read_file(Filename),
    AchievementsAsMaps = jiffy:decode(Content, [return_maps]),
    lists:map(fun achievement:from_map/1, AchievementsAsMaps).
