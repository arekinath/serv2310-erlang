%% @doc Server that keeps the scores and stats of all players
%% @author arekinath
-module(scores).
-behaviour(gen_server).
-export([get/2, start_link/0, start_link/1, reset/1, incr/3, get_all/1, initscore/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(stats, {played=0, won=0, disc=0, score=0}).

%% @doc Starts the scores server, linked to the calling process
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Starts the scores server with a given local/global name, linked to the calling process
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%% @doc Get the stats record for the PlayerName
get(Pid, PlayerName) ->
    gen_server:call(Pid, {get, PlayerName}, 1000).

%% @doc Initialise the score for the PlayerName
initscore(Pid, PlayerName) ->
    gen_server:cast(Pid, {init, PlayerName}).

%% @doc Get all stats records as a list of {Name, Stats}.
get_all(Pid) ->
    gen_server:call(Pid, get_all).

%% @doc Empty the stats
reset(Pid) ->
    gen_server:call(Pid, reset).

%% @doc Increment the stats field Term for the PlayerName.
%%      Term can be one of {played,won,disc,score}.
incr(Pid, PlayerName, Term) ->
    gen_server:cast(Pid, {incr, PlayerName, Term}).

%% gen_server callbacks

init(_Args) ->
    {ok, dict:new()}.

handle_call({get, PlayerName}, _From, State) ->
    {Stats, NewState} = retrieve_or_new(PlayerName, State),
    {reply, Stats, NewState};

handle_call(get_all, _From, State) ->
    {reply, dict:to_list(State), State};

handle_call(reset, _From, _State) ->
    {reply, ok, dict:new()}.

handle_cast({init, PlayerName}, State) ->
    {_Stats, NewState} = retrieve_or_new(PlayerName, State),
    {noreply, NewState};

handle_cast({incr, PlayerName, played}, State) ->
    {Stats, AfterGetState} = retrieve_or_new(PlayerName, State),
    NewStats = Stats#stats{played = Stats#stats.played + 1},
    FinalState = dict:store(PlayerName, NewStats, AfterGetState),
    {noreply, FinalState};
handle_cast({incr, PlayerName, won}, State) ->
    {Stats, AfterGetState} = retrieve_or_new(PlayerName, State),
    NewStats = Stats#stats{won = Stats#stats.won + 1},
    FinalState = dict:store(PlayerName, NewStats, AfterGetState),
    {noreply, FinalState};
handle_cast({incr, PlayerName, disc}, State) ->
    {Stats, AfterGetState} = retrieve_or_new(PlayerName, State),
    NewStats = Stats#stats{disc = Stats#stats.disc + 1},
    FinalState = dict:store(PlayerName, NewStats, AfterGetState),
    {noreply, FinalState};
handle_cast({incr, PlayerName, score}, State) ->
    {Stats, AfterGetState} = retrieve_or_new(PlayerName, State),
    NewStats = Stats#stats{score = Stats#stats.score + 1},
    FinalState = dict:store(PlayerName, NewStats, AfterGetState),
    {noreply, FinalState}.

handle_info(Msg, State) ->
    io:format("scores_server got unexpected message: ~p~n", Msg),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Either retrieves the current stats entry for PlayerName or
%%      creates a new one.
retrieve_or_new(PlayerName, State) ->
    case dict:find(PlayerName, State) of
        error ->
            {#stats{}, dict:store(PlayerName, #stats{}, State)};
        {ok, Value} ->
            {Value, State}
    end.
