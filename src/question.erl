-module(question).
-behaviour(gen_server).
-export([start_link/4, answer/3, get_lines/1, get_options/1, signup/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Starts a question server with the given Question
start_link(Question, Game, Players, Time) ->
    gen_server:start_link(?MODULE, [Question, Game, Players, Time], []).

%% @doc Registers the answer Answer as the attempt by the given player
answer(Pid, Name, Answer) ->
    gen_server:cast(Pid, {answer, Name, Answer}).

%% @doc Signs a player up for this question
signup(Pid, Name) ->
    gen_server:cast(Pid, {signup, Name}).

%% @doc Gets the lines in the question text as a list
get_lines(Pid) ->
    gen_server:call(Pid, get_lines).

%% @doc Gets the answer options for the question as a list of binaries
get_options(Pid) ->
    gen_server:call(Pid, get_options).

%% gen_server callbacks

-record(state, {game, players, lines, options, correct, status=dict:new()}).

init([{Lines, Options, Correct}, Game, Players, Time]) ->
    lists:foreach(fun(Player) ->
        player:send_scores(Player),
        player:start_question(Player, self())
    end, Players),

    timer:send_after(Time*1000, self(), timeout),

    {ok, #state{lines=Lines, options=Options, correct=Correct, game=Game, players=Players}}.

handle_call(get_lines, _From, State) ->
    #state{lines = Lines} = State,
    {reply, Lines, State};

handle_call(get_options, _From, State) ->
    #state{options = Options} = State,
    MapFun = fun({_, _, Bin}) -> Bin end,
    {reply, lists:map(MapFun, Options), State}.

handle_cast({answer, Name, Answer}, State) ->
    #state{correct = Correct, status = Status} = State,
    if Answer =:= Correct ->
        NewStatus = dict:store(Name, correct, Status);
    true ->
        NewStatus = dict:store(Name, incorrect, Status)
    end,
    {noreply, State#state{status = NewStatus}};

handle_cast({signup, Name}, State) ->
    #state{status = Status} = State,
    NewStatus = dict:store(Name, timedout, Status),
    {noreply, State#state{status = NewStatus}}.

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info(Msg, State) ->
    io:format("scores_server got unexpected message: ~p~n", Msg),
    {noreply, State}.

terminate(normal, State) ->
    #state{game = Game, status = Status, players = Players} = State,
    lists:foreach(fun(Player) ->
        player:end_question(Player)
    end, Players),
    game:question_finished(Game, dict:to_list(Status)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
