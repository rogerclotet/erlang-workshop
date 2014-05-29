-module(process).

-export([start/0, terminate/1, loop/1, send/1]).

-record(state, {data}).

start() ->
    State = #state{data = default},
    Pid = spawn_link(process, loop, [State]),
    register(destprocess, Pid).

loop(State) ->
    timer:sleep(1000),

    receive
        {set, data, Data} ->
            io:format("Data is ~p~n", [State#state.data]),
            loop(State#state{data = Data});
        {get, data} ->
            io:format("Data is ~p~n", [State#state.data]),
            State#state.data;
        terminate ->
            exit(reason);
        Msg ->
            io:format("received ~p~n", [Msg])
    end,
    loop(State).

terminate(Pid) ->
    exit(Pid, reason).

send(Message) ->
    destprocess ! Message.
