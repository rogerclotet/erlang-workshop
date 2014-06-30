-module(freq_client).

-behaviour(gen_fsm).

%% API
-export([start_link/1, connect/1, disconnect/1]).

%% gen_fsm callbacks
-export([init/1,
    not_connected/2,
    connected/2,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(FREQ_SERVER, freq_server).
-define(HEARTBEAT_INTERVAL, 5000).

-record(state, {}).

%% API
start_link(Name) ->
    gen_fsm:start_link({global, Name}, ?MODULE, [], []).

connect(Name) ->
    gen_fsm:send_event({global, Name}, connect).

disconnect(Name) ->
    gen_fsm:send_event({global, Name}, disconnect).

%% gen_fsm callbacks
init(_Args) ->
    timer:send_interval(?HEARTBEAT_INTERVAL, send_heartbeat),
    {ok, not_connected, #state{}}.

not_connected(connect, State) ->
    {ok, _} = gen_server:call(?FREQ_SERVER, allocate),
    io:format("Connected~n"),
    {next_state, connected, State};
not_connected(_Event, State) ->
    {next_state, not_connected, State}.

connected(disconnect, State) ->
    gen_server:call(?FREQ_SERVER, deallocate),
    io:format("Not connected~n"),
    {next_state, not_connected, State};
connected(frequency_expired, State) ->
    io:format("Frequency expired~n"),
    {next_state, not_connected, State};
connected(send_heartbeat, State) ->
    {ok, _} = gen_server:call(?FREQ_SERVER, heartbeat),
    {next_state, connected, State};
connected(_Event, State) ->
    {next_state, connected, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
