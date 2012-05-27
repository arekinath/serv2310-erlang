-module(tcp_acceptor).
-export([start_link/3, accept_loop/3]).

%% @doc Starts a tcp acceptor on the given port. It will create players that connect to
%%      the given game and scores servers.
start_link(Port, Game, Scores) ->
    case gen_tcp:listen(Port, [{packet_size, 32000}, {recbuf, 32000}, {active, true}, {packet, line}, binary, {reuseaddr, true}]) of
        {error, Term} ->
            {error, Term};
        {ok, Socket} ->
            {ok, spawn_link(?MODULE, accept_loop, [Socket, Game, Scores])}
    end.

%% @doc Accepts on a socket, creating players.
accept_loop(ListenSocket, Game, Scores) ->
    case gen_tcp:accept(ListenSocket) of
        {error, _} ->
            accept_loop(ListenSocket, Game, Scores);
        {ok, AcceptSocket} ->
            case player:start(AcceptSocket, Game, Scores) of
                {ok, Pid} ->
                    game:take_link(Game, Pid),
                    ok;
                Other ->
                    io:format("accept_loop error spawning player: ~p~n", [Other]),
                    gen_tcp:close(AcceptSocket)
            end,
            accept_loop(ListenSocket, Game, Scores)
    end.
