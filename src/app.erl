%% @doc Application startup module and supervisor
%% @author arekinath
-module(app).

-export([start/1, start/0]).

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

        erlang:process_flag(trap_exit, true),

        LoopFun = fun(Self, Pid) ->
            receive
                {timeout} ->
                    NewPid = game_serv:start_link(RoundTime, QsPerGame, Min, Max, Port, Fname),
                    Self(Self, NewPid);
                {'EXIT', Pid, Reason} ->
                    case Reason of
                        {eaddrinuse, _} ->
                            io:format("Bad Listen: address in use: ~p~n", [Reason]),
                            erlang:halt(6);
                        Other ->
                            io:format("WARNING: restarting gameserv in 1sec: ~p~n", [Other]),
                            timer:send_after(1000, self(), {timeout}),
                            Self(Self, none)
                    end
            end
        end,
        NewPid = game_serv:start_link(RoundTime, QsPerGame, Min, Max, Port, Fname),
        LoopFun(LoopFun, NewPid)
    end.
