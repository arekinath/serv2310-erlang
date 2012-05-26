-module(question_parser).
-export([load/2]).
-record(state, {lines=[], optnum=1, maxopt=none, correct=none, options=[], at=lines}).

binrev(B) ->
    binary:list_to_bin(lists:reverse(binary:bin_to_list(B))).

load(QServ, F, State) when State#state.at =:= lines ->
    case file:read_line(F) of
        eof ->
            done;
        {ok, <<"----\n">>} ->
            NewState = State#state{lines = lists:reverse(State#state.lines), at = opthead},
            load(QServ, F, NewState);
        {ok, Line} ->
            NewLines = [Line | State#state.lines],
            NewState = State#state{lines = NewLines},
            load(QServ, F, NewState)
    end;
load(QServ, F, State) when State#state.at =:= opthead ->
    case file:read_line(F) of
        eof ->
            done;
        {ok, LineWithEnd} ->
            <<"\n", LineRev/binary>> = binrev(LineWithEnd),
            Line = binrev(LineRev),
            [OptCount, Correct] = binary:split(Line, <<" ">>),
            NewState = State#state{at = opts, correct = Correct, maxopt=OptCount},
            load(QServ, F, NewState)
    end;
load(QServ, F, State) when State#state.at =:= opts ->
    case file:read_line(F) of
        eof ->
            OptNum = binary:list_to_bin(integer_to_list(State#state.optnum - 1)),
            if OptNum =:= State#state.maxopt ->
                QServ ! {self(), add, State#state.lines, lists:reverse(State#state.options), State#state.correct},
                receive
                    {QServ, ok} ->
                        done
                end;
            true ->
                done
            end;
        {ok, <<"\n">>} ->
            QServ ! {self(), add, State#state.lines, lists:reverse(State#state.options), State#state.correct},
            receive
                {QServ, ok} ->
                    load(QServ, F, #state{})
            end;
        {ok, LineWithEnd} ->
            <<"\n", LineRev/binary>> = binrev(LineWithEnd),
            Line = binrev(LineRev),
            OptNum = binary:list_to_bin(integer_to_list(State#state.optnum)),
            Correct = if OptNum =:= State#state.correct -> correct;
                         true -> incorrect
                      end,
            Option = {OptNum, Correct, Line},
            NewOpts = [Option | State#state.options],
            NewState = State#state{optnum = State#state.optnum + 1,
                                   options = NewOpts},
            load(QServ, F, NewState)
    end.

load(QServ, Fname) ->
    {ok, F} = file:open(Fname, [read, binary]),
    load(QServ, F, #state{}).
