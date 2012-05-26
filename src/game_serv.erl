-module(game_serv).
-export([loop/1, start_link/6]).
-record(params, {round_time, qspergame, min, max, port, fname}).
-record(state, {qserv, tcpserv, done=0, q=none, clients=[], stats=dict:new(), round_timer=none}).
-record(stats, {played=0, won=0, disc=0, score=0}).

send_all([], _) ->
    done;
send_all(PidList, Msg) ->
    [Pid | Rest] = PidList,
    Pid ! Msg,
    send_all(Rest, Msg).

winners(Stats) ->
    List = dict:to_list(Stats),
    Scores = lists:map(fun({_, #stats{score = Score}}) -> Score end, List),
    MaxScore = lists:max(Scores),
    {Winners, _} = lists:partition(fun({_, #stats{score = Score}}) -> Score =:= MaxScore end, List),
    lists:map(fun({Player, _}) -> Player end, Winners).

update_with_winners(Stats) ->
    Winners = winners(Stats),
    Fun = fun(Name, StatsIn) ->
        MyStats = dict:fetch(Name, StatsIn),
        MyNewStats = MyStats#stats{won = MyStats#stats.won + 1},
        dict:store(Name, MyNewStats, StatsIn)
    end,
    lists:foldl(Fun, Stats, Winners).

client_status(Clients) ->
    Fun = fun(Client) ->
        Client ! {self(), status},
        receive
            {Client, status, Name, Word} ->
                {Name, Word}
        end
    end,
    lists:map(Fun, Clients).

loop(Params, State) when State#state.q =:= none ->
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
            error(no_questions);

        {QServ, question, Lines, Answers, Correct} ->
            Q = {Lines, Answers, Correct},
            send_all(State#state.clients, {self(), scores, dict:to_list(State#state.stats)}),
            send_all(State#state.clients, {self(), question, Q}),
            loop(Params, State#state{q = Q})
    end;
loop(Params, State) ->
    TcpServ = State#state.tcpserv,
    receive
        {'EXIT', TcpServ, Reason} ->
            io:format("WARNING: tcp server on port ~p died: ~p~n", [Params#params.port, Reason]),
            NewTcpServ = triv_tcp_serv:start_link(Params#params.port, self()),
            loop(Params, State#state{tcpserv = NewTcpServ});

        {Pid, score_of, PlayerName} ->
            Stats = dict:fetch(PlayerName, State#state.stats),
            Pid ! {self(), score_of, PlayerName, Stats#stats.score},
            loop(Params, State);

        {Pid, scores} ->
            Pid ! {self(), scores, dict:to_list(State#state.stats)},
            loop(Params, State);

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

        {round_timeout} ->
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

        {Pid, exit_player, Name} ->
            Stats = dict:fetch(Name, State#state.stats),
            NewStats = dict:store(Name, Stats#stats{disc = Stats#stats.disc + 1}, State#state.stats),
            NewState = State#state{clients = State#state.clients -- [Pid],
                                   stats = NewStats},
            Pid ! {self(), ok},
            loop(Params, NewState)
    end.

loop(Params) ->
    erlang:process_flag(trap_exit, true),
    QServ = question_serv:start_link(),
    TcpServ = triv_tcp_serv:start_link(Params#params.port, self()),
    question_parser:load(QServ, Params#params.fname),
    loop(Params, #state{qserv = QServ,
                        tcpserv = TcpServ}).

start_link(RoundTime, QsPerGame, MinPlayers, MaxPlayers, Port, Fname) ->
    Params = #params{round_time = RoundTime, min = MinPlayers, qspergame = QsPerGame,
                     max = MaxPlayers, port = Port, fname = Fname},
    spawn_link(?MODULE, loop, [Params]).
