-module(ack).

-export([start/2]).

start(N, Message) ->
    Receiver = spawn(fun() -> receiver() end),
    spawn(fun() -> sender(N, Message, Receiver) end),
    ok.

sender(0, _, _) -> done;
sender(N, Message, Pid) ->
    io:format("Sending ~p to ~p ~p~n", [Message, Pid, N]),
    Pid ! {message, Message, self()},
    receive
        ack ->
            io:format("Received ack!~n"),
            sender(N - 1, Message, Pid)
    end.

receiver() ->
    receive
        {message, Message, Pid} ->
            io:format("Received ~p~n", [Message]),
            Pid ! ack,
            receiver()
    after 
        5000 -> 
            io:format("Receiver dying for timeout~n")
    end.
