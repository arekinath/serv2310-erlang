%% @doc Question server, stores the list of available questions and dispenses them
%% @author arekinath
-module(question_serv).

-export([start_link/0, loop/0]).

-record(question, {lines, answers, correct}).

%% @doc Starts the server, linking it to the calling process
start_link() ->
    spawn_link(?MODULE, loop, []).

loop() ->
    loop([]).
loop(Questions) ->
    receive
        {Pid, add, Lines, Answers, Correct} ->
            NewQuestions = [#question{lines = Lines, answers = Answers, correct = Correct} | Questions],
            Pid ! {self(), ok},
            loop(NewQuestions);

        {Pid, get_random} ->
            if length(Questions) =:= 0 ->
                Pid ! {self(), no_questions};
            true ->
                Index = random:uniform(length(Questions)),
                Q = lists:nth(Index, Questions),
                Pid ! {self(), question, Q#question.lines, Q#question.answers, Q#question.correct}
            end,
            loop(Questions);

        {Pid, count} ->
            Pid ! {self(), count, length(Questions)},
            loop(Questions)
    end.


