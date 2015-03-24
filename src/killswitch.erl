-module(killswitch).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([is_readonly/0]).
-export([is_readwrite/0]).
-export([set_readonly/0]).
-export([set_readwrite/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    global_ks = rw
}).

%% API.
%% possible evolutions: ets table with per-feature switch

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

is_readonly() ->
    State = gen_server:call(?MODULE, get),
    State =:= ro.

is_readwrite() ->
    State = gen_server:call(?MODULE, get),
    State =:= rw.

set_readonly() ->
    gen_server:cast(?MODULE, {set_switch, ro}).

set_readwrite() ->
    gen_server:cast(?MODULE, {set_switch, rw}).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(get, _From, State) ->
    {reply, State#state.global_ks, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({set_switch, ro}, _) ->
    {noreply, #state{ global_ks = ro }};
handle_cast({set_switch, rw}, _) ->
    {noreply, #state{ global_ks = rw }};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

killswitch_test_() -> 
    {setup,
    fun() -> 
        {ok, Pid} = killswitch:start_link(),
        Pid
    end,
    fun(Pid) ->
        exit(Pid, normal)
    end,
    fun(_) ->
        {generator, fun() -> [
            {"default is readwrite", 
                ?_assert(killswitch:is_readwrite()) },
            {"can set readonly", fun() -> 
                killswitch:set_readonly(),
                ?assert(killswitch:is_readonly())
            end },
            {"can set readonly then reset to readwrite",
                fun() ->
                    killswitch:set_readonly(),
                    killswitch:set_readwrite(),
                    ?assert(killswitch:is_readwrite())
                end }
        ] end }
    end
    }.

-endif.