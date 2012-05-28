-module(game).
-behaviour(gen_server).
-export([start_link/7, question_finished/3, player_connect/2, player_disconnect/2, take_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Starts the game server, linked to the calling process
start_link(Min, Max, Qs, Time, Port, Fname, Scores) ->
    gen_server:start_link(?MODULE, [Min, Max, Qs, Time, Port, Fname, Scores], []).

%% @doc Called by the question proc when it's finished
question_finished(Pid, Players, StatusList) ->
    gen_server:cast(Pid, {question_finished, Players, StatusList}).

%% @doc Tries to connect a new player. Returns {ok, Count, Min} on success, or 'full'
player_connect(Pid, Name) ->
    gen_server:call(Pid, {player_connect, Name}).

%% @doc Called to inform the game that a player has disconnected
player_disconnect(Pid, Name) ->
    gen_server:cast(Pid, {player_disconnect, self(), Name}).

%% @doc Ask the game server to link to a pid
take_link(Pid, Target) ->
    gen_server:cast(Pid, {take_link, Target}).

%% gen_server callbacks

-record(state, {min, max, maxqs, time, round=1, port, qdb, fname, scores, tcp, players=[], question=none, clients=[]}).

init([Min, Max, Qs, Time, Port, Fname, Scores]) ->
    process_flag(trap_exit, true),
    case tcp_acceptor:start_link(Port, self(), Scores) of
        {ok, Tcp} ->
            case question_db:start_link() of
                {ok, Qdb} ->
                    qfile_parser:load(Qdb, Fname),
                    error_logger:info_report([{event, game_startup},{port, Port},{questions_loaded,question_db:count(Qdb)}]),
                    {ok, #state{min=Min, max=Max, maxqs=Qs, port=Port, fname=Fname, scores=Scores, tcp=Tcp, qdb=Qdb, time=Time}};
                Other ->
                    error(Other)
            end;
        {error, eaddrinuse} ->
            io:format("Address in use~n"),
            erlang:halt(6),
            error(eaddrinuse);
        {error, Term} ->
            error(Term)
    end.

handle_call({player_connect, Name}, {Pid, _Ref}, State) ->
    #state{players = Players, min = Min, max = Max, question = Question, qdb = Qdb, time = Time, round = Round, scores = Scores} = State,
    if length(Players) + 1 >= Max ->
        {reply, full, State};

    true ->
        scores:incr(Scores, Name, played),

        NewPlayers = [Pid | Players],
        if (Question =:= none) and (length(Players) + 1 >= Min) ->
            error_logger:info_report([{event, round_start}, {round, Round}, {players, length(Players)+1}]),
            {ok, Q} = question_db:get_random(Qdb),
            {ok, NewQuestion} = question:start_link(Q, self(), NewPlayers, Time),
            NewRound = Round + 1;
        true ->
            NewQuestion = Question,
            NewRound = Round
        end,

        NewState = State#state{players = NewPlayers, question = NewQuestion, round = NewRound},
        {reply, {ok, length(Players) + 1, Min}, NewState}
    end.

handle_cast({question_finished, QPlayers, StatusList}, State) ->
    #state{players = Players, min = Min, scores = Scores, round = Round, maxqs = MaxQs, qdb = Qdb, time = Time} = State,
    % Update the scores of the players
    lists:foreach(fun({Player, Status}) ->
        if Status =:= correct ->
            scores:incr(Scores, Player, score);
        true ->
            no
        end
    end, StatusList),

    if Round > MaxQs ->
        % Game over! Find the winners
        PScores = lists:map(fun({Player, _Status}) ->
            {stats, _, _, _, Score} = scores:get(Scores, Player),
            Score
        end, StatusList),

        MaxScore = if length(PScores) > 0 ->
            lists:max(PScores);
        true ->
            -1
        end,

        Winners = lists:foldl(fun({Player, _}, Acc) ->
            {stats, _, _, _, Score} = scores:get(Scores, Player),
            if Score =:= MaxScore ->
                [Player | Acc];
            true ->
                Acc
            end
        end, [], StatusList),

        % Update the stats for the winners
        lists:foreach(fun(Name) ->
            scores:incr(Scores, Name, won)
        end, Winners),

        error_logger:info_report([{event, game_over}, {players, length(Players)}, {winners, Winners}]),

        % Now send everything to the players
        lists:foreach(fun(Player) ->
            player:send_winners(Player, Winners),
            player:send_correct(Player, StatusList),
            player:send_scores(Player),
            player:disconnect(Player)
        end, QPlayers),

        {noreply, State#state{round = 1, players = Players -- QPlayers, question = none}};

    true ->
        lists:foreach(fun(Player) ->
            player:send_correct(Player, StatusList)
        end, QPlayers),

        if length(Players) >= Min ->
            {ok, Q} = question_db:get_random(Qdb),
            {ok, Question} = question:start_link(Q, self(), Players, Time),
            error_logger:info_report([{event, round_start}, {round, Round}, {players, length(Players)}]),
            {noreply, State#state{round = Round + 1, question = Question}};
        true ->
            {noreply, State#state{question = none}}
        end
    end;

handle_cast({player_disconnect, Pid, Name}, State) ->
    #state{players = Players, scores = Scores} = State,
    scores:incr(Scores, Name, disc),
    {noreply, State#state{players = Players -- [Pid]}};

handle_cast({take_link, Pid}, State) ->
    #state{clients = Clients} = State,
    link(Pid),
    {noreply, State#state{clients = [Pid | Clients]}}.

handle_info({'EXIT', Pid, Reason}, State) ->
    #state{players = Players, clients = Clients, tcp = Tcp, port = Port, scores = Scores} = State,

    IsPlayer = lists:member(Pid, Players),
    IsClient = lists:member(Pid, Clients),

    if IsPlayer =:= true ->
        error_logger:info_report([{event, player_died},{pid, Pid},{reason, Reason}]),
        {noreply, State#state{players = Players -- [Pid]}};

    IsClient =:= true ->
        {noreply, State#state{clients = Clients -- [Pid]}};

    Tcp =:= Pid ->
        error_logger:info_report([{event, tcp_died},{pid, Pid},{reason,Reason},{attempt_restart_in, 1000}]),
        receive
        after 1000 ->
            {ok, NewTcp} = tcp_acceptor:start_link(Port, self(), Scores),
            {noreply, State#state{tcp = NewTcp}}
        end;

    Reason =:= normal ->
        {noreply, State};

    true ->
        {stop, {child_died, Pid, Reason}, State}
    end;

handle_info(Msg, State) ->
    io:format("game server got unexpected message: ~p~n", Msg),
    {noreply, State}.

terminate({child_died, _, _}, _State) ->
    ok;
terminate(normal, _State) ->
    ok;
terminate(shutdown, State) ->
    #state{players = Players} = State,
    lists:foreach(fun(Player) ->
        player:disconnect(Player)
    end, Players),
    error_logger:info_report([{event, game_shutdown}]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
