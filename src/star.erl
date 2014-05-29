-module(star).

-export([start/2]).

%% Creates a star with N processes connected to him
%% and pass to each one a message M times.
start(N, M) -> start_processes(N, N, M).

start_processes(NTotal, 0, _) ->
    resend_messages(NTotal - 1);
start_processes(NTotal, N, M) ->
    Pid = spawn(fun () -> listen() end),
    Pid ! {message, M, self()},
    start_processes(NTotal, N - 1, M).

resend_messages(0) -> ok;
resend_messages(N) ->
    receive
        {ack, 0, _} ->
            resend_messages(N - 1);
        {ack, M, Pid} ->
            io:format("Received ack~n"),
            Pid ! {message, M - 1, self()},
            resend_messages(N)
    end.

listen() ->
    receive
        {message, M, Pid} ->
            io:format("Received message (~p missing)~n", [M]),
            Pid ! {ack, M, self()},
            if
                M =:= 0 -> ok;
                true -> listen()
            end
    end.
