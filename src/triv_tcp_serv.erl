%% @doc TCP server, listens on a given port and handles each client
%% @author arekinath
-module(triv_tcp_serv).

-export([start_link/2, accept_loop/2, talk_loop/2]).

-record(state, {
    name,               % name the client logged in with
    q=none,             % current question
    status=timedout     % last answer status (timedout, incorrect, correct)
    }).

%% @doc Starts the TCP server on Port, linking it to the calling process
start_link(Port, GameServ) ->
    {ok, Socket} = gen_tcp:listen(Port, [{active, true}, {packet, line}, binary]),
    spawn_link(?MODULE, accept_loop, [Socket, GameServ]).

%% @doc Loop for the process accepting new connections
accept_loop(ListenSocket, GameServ) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    Pid = spawn(?MODULE, talk_loop, [AcceptSocket, GameServ]),
    gen_tcp:controlling_process(AcceptSocket, Pid),
    accept_loop(ListenSocket, GameServ).

%% @doc Loop run in each individual client process
talk_loop(Socket, GameServ) ->
    talk_loop(Socket, GameServ, #state{name = none}).

% Before we get the first line (no name set)
talk_loop(Socket, GameServ, #state{name = none}) ->
    receive
        {tcp, Socket, <<"scores\n">>} ->
            GameServ ! {self(), scores},
            receive
                {GameServ, scores, Scores} ->
                    gen_tcp:send(Socket, scores_to_bin(Scores))
            end,
            gen_tcp:close(Socket);

        {tcp, Socket, Line} ->
            % Strip off the newline
            <<"\n", TextRev/binary>> = binrev(Line),
            Name = binrev(TextRev),

            if Name =:= <<"">> ->
                % Empty names are a protocol violation
                gen_tcp:close(Socket);

            true ->
                GameServ ! {self(), new_player, Name},
                receive
                    {GameServ, ok, Count, Min} ->
                        CountBin = binary:list_to_bin(integer_to_list(Count)),
                        MinBin = binary:list_to_bin(integer_to_list(Min)),
                        gen_tcp:send(Socket, <<"Hello ", Name/binary, " ", CountBin/binary, "/", MinBin/binary, "\n">>),
                        talk_loop(Socket, GameServ, #state{name = Name});

                    {GameServ, full} ->
                        gen_tcp:send(Socket, <<"$full, sorry\n">>),
                        gen_tcp:close(Socket)
                end
            end;

        {tcp_closed, Socket} ->
            gen_tcp:close(Socket)
    end;

% After we've started playing the game
talk_loop(Socket, GameServ, State) ->
    receive
        {GameServ, status} ->
            GameServ ! {self(), status, State#state.name, State#state.status},
            talk_loop(Socket, GameServ, State);

        {GameServ, correct, Corrects} ->
            MapFun = fun({Name, Status}) ->
                StatusBin = case Status of
                    correct -> <<"Correct">>;
                    incorrect -> <<"Incorrect">>;
                    timedout -> <<"TimedOut">>;
                    true -> <<"???">>
                end,
                <<Name/binary, ":", StatusBin/binary>>
            end,
            CBin = bin_concat(lists:map(MapFun, Corrects), <<" ">>),
            gen_tcp:send(Socket, <<"C ", CBin/binary, "\n">>),
            talk_loop(Socket, GameServ, State);

        {GameServ, scores, Scores} ->
            ScoresBin = scores_min_to_bin(Scores),
            gen_tcp:send(Socket, <<"S ", ScoresBin/binary, "\n">>),
            talk_loop(Socket, GameServ, State);

        {GameServ, question, Q} ->
            gen_tcp:send(Socket, question_to_bin(Q)),
            talk_loop(Socket, GameServ, State#state{q = Q, status=timedout});

        {GameServ, winners, Winners} ->
            WinBin = bin_concat(Winners, <<" ">>),
            gen_tcp:send(Socket, <<"W ", WinBin/binary, "\n">>),
            talk_loop(Socket, GameServ, State);

        {GameServ, bye} ->
            gen_tcp:close(Socket);

        {tcp, Socket, LineWithEnd} ->
            % Strip off the newline
            <<"\n", LineRev/binary>> = binrev(LineWithEnd),
            Line = binrev(LineRev),

            if State#state.q =:= none ->
                % We're not in a question yet! this is bad protocol
                GameServ ! {self(), exit_player, State#state.name},
                receive
                    {GameServ, ok} ->
                        gen_tcp:close(Socket)
                end;

            true ->
                {_, _, Correct} = State#state.q,
                if Line =:= Correct ->
                    talk_loop(Socket, GameServ, State#state{status=correct});
                true ->
                    talk_loop(Socket, GameServ, State#state{status=incorrect})
                end
            end;

        {tcp_closed, Socket} ->
            GameServ ! {self(), exit_player, State#state.name},
            receive
                {GameServ, ok} ->
                    gen_tcp:close(Socket)
            end
    end.


%% @doc Reverses a binary
binrev(B) ->
    binary:list_to_bin(lists:reverse(binary:bin_to_list(B))).

%% @doc Concatenates the binaries in List
bin_concat(List) ->
    bin_concat(<<"">>, <<"">>, List).
%% @doc Concatenates the binaries in List, putting Sep between each one
bin_concat(List, Sep) ->
    bin_concat(<<"">>, Sep, List).
bin_concat(Bin, _, []) ->
    Bin;
bin_concat(<<"">>, Sep, List) ->
    [Head | Tail] = List,
    bin_concat(Head, Sep, Tail);
bin_concat(Bin, Sep, List) ->
    [Head | Tail] = List,
    NewBin = <<Bin/binary, Sep/binary, Head/binary>>,
    bin_concat(NewBin, Sep, Tail).

%% @doc Converts a list of {Player, #stats} into the binary representation used
%%      when a 'scores'-only client connects to the server
scores_to_bin(Scores) ->
    scores_to_bin(<<"">>, Scores).
scores_to_bin(Bin, []) ->
    Bin;
scores_to_bin(Bin, Scores) ->
    [{Player, {stats, Played, Won, Disc, Score}} | Rest] = Scores,
    PlayedBin = binary:list_to_bin(integer_to_list(Played)),
    WonBin = binary:list_to_bin(integer_to_list(Won)),
    DiscBin = binary:list_to_bin(integer_to_list(Disc)),
    ScoreBin = binary:list_to_bin(integer_to_list(Score)),
    NewBin = <<Bin/binary, Player/binary,
                           " played:", PlayedBin/binary,
                           " won:", WonBin/binary,
                           " disc:", DiscBin/binary,
                           " score:", ScoreBin/binary,
                           "\n">>,
    scores_to_bin(NewBin, Rest).

%% @doc Converts a list of {Player, #stats} into the binary representation sent
%%      to clients during a game.
scores_min_to_bin(Scores) ->
    MapFun = fun({Player, {stats, _, _, _, Score}}) ->
        ScoreBin = binary:list_to_bin(integer_to_list(Score)),
        <<Player/binary, ":", ScoreBin/binary>>
    end,
    bin_concat(lists:map(MapFun, Scores), <<" ">>).

%% @doc Converts a question tuple {Lines,Answers,Correct} into the binary representation
%%      sent to clients playing the game.
question_to_bin(Question) ->
    {Lines, Answers, _} = Question,
    LinesBin = bin_concat(Lines),
    CountBin = binary:list_to_bin(integer_to_list(length(Answers))),
    MapFun = fun({_, _, Bin}) -> Bin end,
    AnswersBin = bin_concat(lists:map(MapFun, Answers), <<"\n">>),
    <<LinesBin/binary, ".\n", CountBin/binary, "\n", AnswersBin/binary, "\n">>.
