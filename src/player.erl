-module(player).
-behaviour(gen_server).
-export([start/3, get_name/1, send_correct/2, send_scores/1, start_question/2, end_question/1, send_winners/2, disconnect/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Starts the player, linked to the calling process
start(Socket, Game, Scores) ->
    case gen_server:start(?MODULE, [Socket, Game, Scores], []) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Socket, Pid),
            timer:send_after(15000, Pid, auth_timeout),
            {ok, Pid};
        Other ->
            Other
    end.

%% @doc Gets the name of the player
get_name(Pid) ->
    gen_server:call(Pid, get_name).

%% @doc Sends the "correct?" line to the player. Corrects  is a list of {Player, Status} tuples.
send_correct(Pid, Corrects) ->
    gen_server:cast(Pid, {send_correct, Corrects}).

%% @doc Sends the 'S' scores line to the player.
send_scores(Pid) ->
    gen_server:cast(Pid, send_scores).

%% @doc Sends a new question to the player and beings accepting guesses.
start_question(Pid, Question) ->
    gen_server:cast(Pid, {start_question, Question}).

%% @doc End of the question, stop accepting guesses
end_question(Pid) ->
    gen_server:cast(Pid, end_question).

%% @doc Send the 'W' winners line. Winners is a tuple of binaries.
send_winners(Pid, Winners) ->
    gen_server:cast(Pid, {send_winners, Winners}).

%% @doc Disconnect the player
disconnect(Pid) ->
    gen_server:cast(Pid, disconnect).

%% gen_server callbacks

-record(state, {name = none, socket, scores, game, question=none}).

init([Socket, Game, Scores]) ->
    {ok, #state{socket = Socket, game = Game, scores = Scores}}.

handle_call(get_name, _From, State) ->
    #state{name = Name} = State,
    {reply, Name, State}.

handle_cast({send_correct, Corrects}, State) ->
    #state{socket = Socket} = State,
    MapFun = fun({Player, Status}) ->
        StatusBin = case Status of
            correct -> <<"Correct">>;
            incorrect -> <<"Incorrect">>;
            timedout -> <<"TimedOut">>;
            _Other -> <<"???">>
        end,
        <<Player/binary, ":", StatusBin/binary>>
    end,
    CorrectsBin = bin_concat(lists:map(MapFun, Corrects), <<" ">>),
    gen_tcp:send(Socket, <<"C", CorrectsBin/binary, "\n">>),
    {noreply, State};

handle_cast(send_scores, State) ->
    #state{socket = Socket, scores = ScoreServ} = State,
    Scores = scores:get_all(ScoreServ),
    MapFun = fun(Stats) -> stats_to_short_bin(Stats) end,
    ScoresBin = bin_concat(lists:map(MapFun, Scores), <<" ">>),
    gen_tcp:send(Socket, <<"S", ScoresBin/binary, "\n">>),
    {noreply, State};

handle_cast({send_winners, Winners}, State) ->
    #state{socket = Socket} = State,
    WinnersBin = bin_concat(Winners, <<" ">>),
    gen_tcp:send(Socket, <<"W", WinnersBin/binary, "\n">>),
    {noreply, State};

handle_cast({start_question, Question}, State) ->
    #state{socket = Socket, name = Name} = State,
    question:signup(Question, Name),

    LinesBin = bin_concat(question:get_lines(Question)),
    Options = question:get_options(Question),
    OptCountBin = binary:list_to_bin(integer_to_list(length(Options))),
    OptsBin = bin_concat(Options, <<"\n">>),
    gen_tcp:send(Socket, <<LinesBin/binary, ".\n", OptCountBin/binary, "\n", OptsBin/binary, "\n">>),

    {noreply, State#state{question = Question}};

handle_cast(end_question, State) ->
    {noreply, State#state{question = none}};

handle_cast(disconnect, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, Data}, State) when State#state.socket =:= Socket ->
    #state{name = Name, game = Game, question = Question, scores = Scores} = State,
    Line = case binrev(Data) of
        <<"\n", LineRev/binary>> ->
            binrev(LineRev);
        _ ->
            gen_tcp:send(Socket, <<"$line too long :(\n">>),
            <<"">>
    end,
    if Name =:= none ->
        if Line =:= <<"">> ->
            {stop, bad_protocol, State};

        Line =:= <<"scores">> ->
            List = scores:get_all(Scores),
            MapFun = fun(Stats) -> stats_to_full_bin(Stats) end,
            gen_tcp:send(Socket, bin_concat(lists:map(MapFun, List))),
            {stop, normal, State};

        Line =:= <<"::reset">> ->
            case scores:reset(Scores) of
                ok ->
                    gen_tcp:send(Socket, <<"ok\n">>),
                    {stop, normal, State};
                _Other ->
                    error("reset failed")
            end;

        true ->
            case game:player_connect(Game, Line) of
                {ok, Count, Max} ->
                    CountBin = binary:list_to_bin(integer_to_list(Count)),
                    MaxBin = binary:list_to_bin(integer_to_list(Max)),
                    gen_tcp:send(Socket, <<"Hello ", Line/binary, " ", CountBin/binary, "/", MaxBin/binary, ".\n">>),
                    {noreply, State#state{name = Line}};
                full ->
                    gen_tcp:send(Socket, <<"$Sorry, game is full.\n">>),
                    {stop, normal, State}
            end
        end;

    Question =:= none ->
        {stop, bad_protocol, State};

    true ->
        question:answer(Question, Name, Line),
        {noreply, State}
    end;

handle_info({tcp_closed, Socket}, State) when State#state.socket =:= Socket ->
    {stop, normal, State};

handle_info(auth_timeout, State) ->
    if State#state.name =:= none ->
        {stop, auth_timeout, State};
    true ->
        {noreply, State}
    end;

handle_info(Msg, State) ->
    io:format("player got unexpected message: ~p~n", Msg),
    {noreply, State}.

terminate(auth_timeout, State) ->
    #state{socket = Socket} = State,
    gen_tcp:send(Socket, <<"$auth timed out\n">>),
    gen_tcp:close(Socket),
    ok;
terminate(normal, #state{socket = Socket, game = Game, name = Name}) ->
    if Name =/= none ->
        game:player_disconnect(Game, Name);
    true ->
        none
    end,
    gen_tcp:close(Socket),
    ok;
terminate(bad_protocol, State) ->
    #state{socket = Socket, name = Name, game = Game} = State,
    if Name =/= none ->
        game:player_disconnect(Game, Name);
    true ->
        none
    end,
    gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Reverses a binary
binrev(B) ->
    binary:list_to_bin(lists:reverse(binary:bin_to_list(B))).

%% @doc Concatenates a list of binaries together
bin_concat(List) ->
    bin_concat(List, <<"">>).

%% @doc Concatenates a list of binaries together separated by Sep
bin_concat([], _Sep) ->
    <<"">>;
bin_concat(List, Sep) ->
    Fun = fun(Bin, Acc) ->
        if Acc =:= first ->
            <<Bin/binary>>;
        true ->
            <<Acc/binary, Sep/binary, Bin/binary>>
        end
    end,
    lists:foldl(Fun, first, List).

%% @doc Converts a stats tuple into a binary for output in the dedicated scores context
stats_to_full_bin(Stats) ->
    {Player, {stats, Played, Won, Disc, Score}} = Stats,
    PlayedBin = binary:list_to_bin(integer_to_list(Played)),
    WonBin = binary:list_to_bin(integer_to_list(Won)),
    DiscBin = binary:list_to_bin(integer_to_list(Disc)),
    ScoreBin = binary:list_to_bin(integer_to_list(Score)),
    <<Player/binary, " played:", PlayedBin/binary,
                     " won:", WonBin/binary,
                     " disc:", DiscBin/binary,
                     " score:", ScoreBin/binary,
                     "\n">>.

%% @doc Converts a stats tuple into a binary for output in the game
stats_to_short_bin(Stats) ->
    {Player, {stats, _, _, _, Score}} = Stats,
    ScoreBin = binary:list_to_bin(integer_to_list(Score)),
    <<Player/binary, ":", ScoreBin/binary>>.
