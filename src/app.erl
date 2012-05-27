%% @doc Application startup module and supervisor
%% @author arekinath
-module(app).
-behaviour(supervisor).

-export([start/1, start/0, init/1]).

start() ->
    io:format("Usage: serv round_time minplayers maxplayers port qfile~n"),
    erlang:halt(1).

start(Args) ->
    if length(Args) =/= 5 ->
        start();
    true ->
        RoundTime = list_to_integer(lists:nth(1, Args)),
        Min = list_to_integer(lists:nth(2, Args)),
        Max = list_to_integer(lists:nth(3, Args)),
        Port = list_to_integer(lists:nth(4, Args)),
        Fname = lists:nth(5, Args),
        QsPerGame = 4,
        case supervisor:start_link(?MODULE, [Min, Max, QsPerGame, RoundTime, Port, Fname]) of
            {ok, _Pid} ->
                done;
            {error, Term} ->
                error(Term)
        end,
        spin()
    end.

spin() -> spin().

init(Args) ->
    {ok, {{one_for_one, 5, 60},
        [
            {scoreserv,
                {scores, start_link, [{global, scoreserv}]},
                permanent,
                5000,
                worker,
                [scores]
            },
            {gameserv,
                {game, start_link, Args ++ [{global, scoreserv}]},
                permanent,
                5000,
                worker,
                [game]
            }
        ]}}.
