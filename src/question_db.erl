%% @doc Question server, stores the list of available questions and dispenses them
%% @author arekinath
-module(question_db).
-behaviour(gen_server).
-export([start_link/0, add/2, count/1, get_at/2, get_random/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Starts the server, linking it to the calling process
start_link() -> gen_server:start_link(?MODULE, [], []).

%% @doc Adds the given question to the database
add(Pid, Question) ->
    gen_server:cast(Pid, {add, Question}).

%% @doc Returns the count of questions currently stored
count(Pid) ->
    gen_server:call(Pid, count).

%% @doc Gets question number #Index. Returns {ok, Question} or 'badindex'
get_at(Pid, Index) ->
    gen_server:call(Pid, {get_at, Index}).

%% @doc Gets a random question from the db. Returns {ok, Question} or 'empty'
get_random(Pid) ->
    gen_server:call(Pid, get_random).

%% gen_server callbacks

init(_Args) ->
    random:seed(now()),
    {ok, []}.

handle_cast({add, Question}, Questions) ->
    {noreply, [Question | Questions]}.

handle_call(count, _From, Questions) ->
    {reply, length(Questions), Questions};

handle_call({get_at, Index}, _From, Questions) ->
    if length(Questions) =< Index ->
        {reply, {ok, lists:nth(Index, Questions)}, Questions};
    true ->
        {reply, badindex, Questions}
    end;

handle_call(get_random, _From, Questions) ->
    if length(Questions) > 0 ->
        Index = random:uniform(length(Questions)),
        {reply, {ok, lists:nth(Index, Questions)}, Questions};
    true ->
        {reply, empty, Questions}
    end.

handle_info(Msg, State) ->
    io:format("question_db got unexpected message: ~p~n", Msg),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

