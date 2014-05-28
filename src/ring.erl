-module(ring).

-export([start/3]).

%% Creates N processes that will pass a message around M times
start(N, M, Message) ->
    NextPid = initialise_ring(N - 1, M, self(), Message),
    ring_node(M, NextPid),
    done.

initialise_ring(0, _, NextPid, Message) ->
    NextPid ! Message,
    NextPid;
initialise_ring(N, M, NextPid, Message) ->
    Pid = spawn_ring_node(M, NextPid),
    initialise_ring(N - 1, M, Pid, Message).

ring_node(0, _) ->
    io:format("Finished passing messages!~n");
ring_node(MessagesLeft, NextPid) ->
    receive
        Message ->
            io:format("Received ~p (~p)~n", [Message, MessagesLeft]),
            NextPid ! Message
    end,
    ring_node(MessagesLeft - 1, NextPid).

spawn_ring_node(MessagesToPass, NextPid) ->
    spawn(fun () -> ring_node(MessagesToPass, NextPid) end).
