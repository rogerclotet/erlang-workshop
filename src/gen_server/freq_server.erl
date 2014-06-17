-module(freq_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {frequencies, used, expire}).
-define(SERVER, ?MODULE).

%% API
start_link(Frequencies, Timeout) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Frequencies, Timeout], []).

%% gen_server callbacks
init([Frequencies, Expire]) ->
    {ok, #state{frequencies = Frequencies, used = [], expire = Expire}}.

handle_call(allocate, {From, _}, State) ->
    {ok, Reply, NewState} = allocate(From, State),
    erlang:send_after(State#state.expire, self(), {expire, From}),
    print_state(NewState),
    {reply, Reply, NewState};
handle_call(deallocate, {From, _}, State) ->
    {ok, NewState} = release_frequencies(From, State),
    print_state(NewState),
    {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({expire, From}, State) ->
    NewState = release_frequencies(From, State),
    io:format("Frequency expired for ~p~n", [From]),
    print_state(NewState),
    gen_fsm:send_event(From, frequency_expired),
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
allocate(From, State = #state{frequencies = [Head | Tail]}) ->
    Used = State#state.used ++ [{Head, From}],
    {ok, {ok, Head}, State#state{frequencies = Tail, used = Used}}.

release_frequencies(From, State) ->
    Removed = [Freq || {Freq, FreqFrom} <- State#state.used, From =:= FreqFrom],
    NewUsed = lists:subtract(State#state.used, Removed),
    NewFrequencies = lists:append(State#state.frequencies, Removed),
    State#state{frequencies = NewFrequencies, used = NewUsed}.

print_state(State) ->
    io:format("~p~n", [State]).
