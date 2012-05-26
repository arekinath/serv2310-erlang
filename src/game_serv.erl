%% @doc Main game server, the top-level server
%% @author arekinath
-module(game_serv).

-export([loop/1, start_link/6]).

-record(params, {round_time, qspergame, min, max, port, fname}).

-record(state, {
    qserv, tcpserv,     % other servers
    done=0,             % number of rounds played so far
    q=none,             % current question
    clients=[],
    stats=dict:new(),
    round_timer=none}).

-record(stats, {played=0, won=0, disc=0, score=0}).

%% @doc Sends the message Msg to all Pids in PidList
send_all([], _) ->
    done;
send_all(PidList, Msg) ->
    [Pid | Rest] = PidList,
    Pid ! Msg,
    send_all(Rest, Msg).

%% @doc Takes a stats dict and returns a list of player names
%%      that have the highest score
winners(Stats) ->
    List = dict:to_list(Stats),
    Scores = lists:map(fun({_, #stats{score = Score}}) -> Score end, List),
    MaxScore = lists:max(Scores),
    {Winners, _} = lists:partition(fun({_, #stats{score = Score}}) -> Score =:= MaxScore end, List),
    lists:map(fun({Player, _}) -> Player end, Winners).

%% @doc Updates the given stats dict with the new "won" counts for
%%      each player, called at the end of a game.
update_with_winners(Stats) ->
    Winners = winners(Stats),
    Fun = fun(Name, StatsIn) ->
        MyStats = dict:fetch(Name, StatsIn),
        MyNewStats = MyStats#stats{won = MyStats#stats.won + 1},
        dict:store(Name, MyNewStats, StatsIn)
    end,
    lists:foldl(Fun, Stats, Winners).

%% @doc Queries the list of clients (pids) for their current status, returns
%%      a list of {Name, Status} tuples.
client_status(Clients) ->
    Fun = fun(Client) ->
        Client ! {self(), status},
        receive
            {Client, status, Name, Word} ->
                {Name, Word}
        end
    end,
    lists:map(Fun, Clients).

%% @doc Starts the game server, linking it to the calling process
start_link(RoundTime, QsPerGame, MinPlayers, MaxPlayers, Port, Fname) ->
    Params = #params{round_time = RoundTime, min = MinPlayers, qspergame = QsPerGame,
                     max = MaxPlayers, port = Port, fname = Fname},
    spawn_link(?MODULE, loop, [Params]).

%% @doc Starts the main loop of the game server
loop(Params) ->
    erlang:process_flag(trap_exit, true),
    QServ = question_serv:start_link(),
    TcpServ = triv_tcp_serv:start_link(Params#params.port, self()),
    question_parser:load(QServ, Params#params.fname),
    loop(Params, #state{qserv = QServ,
                        tcpserv = TcpServ}).

%% @doc The main loop of the game server
loop(Params, State) when State#state.q =:= none ->
    % We have no question, so ask the QServ for one
    QServ = State#state.qserv,
    QServ ! {self(), get_random},
    TcpServ = State#state.tcpserv,
    receive
        {'EXIT', QServ, Reason} ->
            io:format("WARNING: qserv died: ~p~n", [Reason]),
            NewQServ = question_serv:start_link(),
            question_parser:load(QServ, Params#params.fname),
            loop(Params, State#state{qserv = NewQServ});

        {'EXIT', TcpServ, Reason} ->
            io:format("WARNING: tcp server on port ~p died: ~p~n", [Params#params.port, Reason]),
            NewTcpServ = triv_tcp_serv:start_link(Params#params.port, self()),
            loop(Params, State#state{tcpserv = NewTcpServ});

        {QServ, no_questions} ->
            % Uh oh!
            error(no_questions);

        {QServ, question, Lines, Answers, Correct} ->
            Q = {Lines, Answers, Correct},
            % This is a no-op the first time around, but when we're on the second or later
            % question in the game, we need to update the clients
            send_all(State#state.clients, {self(), scores, dict:to_list(State#state.stats)}),
            send_all(State#state.clients, {self(), question, Q}),
            loop(Params, State#state{q = Q})
    end;

% Only when we have a question
loop(Params, State) ->
    TcpServ = State#state.tcpserv,
    receive
        {'EXIT', TcpServ, Reason} ->
            io:format("WARNING: tcp server on port ~p died: ~p~n", [Params#params.port, Reason]),
            NewTcpServ = triv_tcp_serv:start_link(Params#params.port, self()),
            loop(Params, State#state{tcpserv = NewTcpServ});

        % Return the list of stats to the sender as {Player, #stats}
        {Pid, scores} ->
            Pid ! {self(), scores, dict:to_list(State#state.stats)},
            loop(Params, State);

        % Sent by a tcp_serv process when the user wants to log in
        % We return with {self(), ok, N, M} or {self(), full}
        {Pid, new_player, Name} ->
            if length(State#state.clients) >= Params#params.max ->
                Pid ! {self(), full},
                loop(Params, State);
            true ->
                Stats = case dict:find(Name, State#state.stats) of
                    error ->
                        #stats{};
                    {ok, Value} ->
                        Value
                    end,
                NewStats = dict:store(Name, Stats#stats{played = Stats#stats.played + 1}, State#state.stats),
                Pid ! {self(), ok, length(State#state.clients)+1, Params#params.min},

                if length(State#state.clients)+1 > Params#params.min ->
                    Pid ! {self(), scores, dict:to_list(State#state.stats)},
                    Pid ! {self(), question, State#state.q},
                    RoundTimer = State#state.round_timer;

                length(State#state.clients)+1 == Params#params.min ->
                    send_all(State#state.clients, {self(), scores, dict:to_list(State#state.stats)}),
                    send_all(State#state.clients, {self(), question, State#state.q}),
                    Pid ! {self(), scores, dict:to_list(State#state.stats)},
                    Pid ! {self(), question, State#state.q},
                    RoundTimer = erlang:send_after(Params#params.round_time * 1000, self(), {round_timeout});

                true ->
                    RoundTimer = none
                end,
                NewState = State#state{clients = [Pid | State#state.clients],
                                       stats = NewStats,
                                       round_timer = RoundTimer},
                loop(Params, NewState)
            end;

        % Received from the timer that goes off when this question has expired
        {round_timeout} ->

            % Find the clients that answered correctly and update their scores
            Status = client_status(State#state.clients),
            Fun = fun({Name, Stat}, StatsIn) ->
                if Stat =:= correct ->
                    MyStats = dict:fetch(Name, StatsIn),
                    MyNewStats = MyStats#stats{score = MyStats#stats.score + 1},
                    dict:store(Name, MyNewStats, StatsIn);
                true ->
                    StatsIn
                end
            end,
            StatsWithCorrect = lists:foldl(Fun, State#state.stats, Status),

            % Check if this is the last question
            if State#state.done + 1 >= Params#params.qspergame ->
                NewStats = update_with_winners(StatsWithCorrect),

                send_all(State#state.clients, {self(), correct, client_status(State#state.clients)}),
                send_all(State#state.clients, {self(), winners, winners(NewStats)}),
                send_all(State#state.clients, {self(), scores, dict:to_list(NewStats)}),
                send_all(State#state.clients, {self(), bye}),

                loop(Params, State#state{stats = NewStats, done = 0, clients = [], q = none, round_timer = none});
            true ->
                send_all(State#state.clients, {self(), correct, client_status(State#state.clients)}),

                RoundTimer = erlang:send_after(Params#params.round_time * 1000, self(), {round_timeout}),
                loop(Params, State#state{stats = StatsWithCorrect, done = State#state.done + 1, q = none, round_timer = RoundTimer})
            end;

        % The player disconnected or sent invalid data
        {Pid, exit_player, Name} ->
            Stats = dict:fetch(Name, State#state.stats),
            NewStats = dict:store(Name, Stats#stats{disc = Stats#stats.disc + 1}, State#state.stats),
            NewState = State#state{clients = State#state.clients -- [Pid],
                                   stats = NewStats},
            Pid ! {self(), ok},
            loop(Params, NewState)
    end.
