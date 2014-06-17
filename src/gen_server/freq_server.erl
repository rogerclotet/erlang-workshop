-module(freq_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {frequencies, used, timeout}).
-record(used_frequency, {freq, ref, pid}).
-define(SERVER, ?MODULE).

%% API
start_link(Frequencies, Timeout) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Frequencies, Timeout], []).

%% gen_server callbacks
init([Frequencies, Timeout]) ->
    {ok, #state{frequencies = Frequencies, used = [], timeout = Timeout}}.

handle_call(allocate, {From, _}, State) ->
    {ok, Reply, NewState} = allocate(From, State),
    print_state(NewState),
    {reply, Reply, NewState};
handle_call(deallocate, {From, _}, State) ->
    {ok, NewState} = release_frequencies(From, State),
    print_state(NewState),
    {reply, ok, NewState};
handle_call({add_frequency, Freq}, _From, State) ->
    Frequencies = State#state.frequencies ++ [Freq], % TODO validation
    {reply, ok, State#state{frequencies = Frequencies}};
handle_call(heartbeat, {From, _}, State) ->
    {ok, NewState} = update_used_frequency(From, State),
    {noreply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, From, Ref}, State) ->
    NewState = timeout_frequencies(From, Ref, State),
    print_state(NewState),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
allocate(_From, State = #state{frequencies = []}) ->
    {ok, {error, no_frequency_available}, State};
allocate(From, State) ->
    {ok, NewState} = reserve_frequency(From, State),
    {ok, ok, NewState}.

reserve_frequency(From, State) ->
    [Freq | Tail] = State#state.frequencies,
    Ref = erlang:make_ref(),
    Used = State#state.used ++ [#used_frequency{freq = Freq, ref = Ref, pid = From}],

    if
        is_number(State#state.timeout) ->
            erlang:send_after(State#state.timeout, self(), {timeout, From, Ref});
        true -> ok
    end,

    State#state{frequencies = Tail, used = Used}.

release_frequencies(From, State) ->
    Removed = [Freq || #used_frequency{freq = Freq, pid = FreqFrom} <- State#state.used, From =:= FreqFrom],
    remove_frequencies(Removed, State).

timeout_frequencies(From, Ref, State) ->
    Removed = [Freq || #used_frequency{freq = Freq, ref = SavedRef, pid = FreqFrom} <- State#state.used, From =:= FreqFrom, Ref =:= SavedRef],
    remove_frequencies(Removed, State).

remove_frequencies(Removed, State) ->
    NewUsed = lists:subtract(State#state.used, Removed),
    NewFrequencies = lists:append(State#state.frequencies, Removed),
    State#state{frequencies = NewFrequencies, used = NewUsed}.

update_used_frequency(From, State) ->
    Ref = erlang:make_ref(),
    lists:map(
        fun (Used = #used_frequency{pid = Pid}) ->
            case Pid of
                From ->
                    Used#used_frequency{ref = Ref};
                _ ->
                    Used
            end
        end,
        State#state.used
    ).

print_state(State) ->
    io:format("~p~n", [State]).
