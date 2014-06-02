-module(kv).

-behaviour(gen_server).

%% Start server
-export([start_link/3]).

%% Server API
-export([set/2, set/3, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {key, value}).

%% GEN_SERVER (kv processes)
start_link(Key, Value, Timeout) ->
    gen_server:start_link({global, get_global(Key)}, ?MODULE, [Key, Value, Timeout], []).

init([Key, Value, Timeout]) ->
    {ok, #state{key = Key, value = Value}, Timeout}.

handle_call(Request, _From, State) ->
    case Request of
        get ->
            {reply, State#state.value, State}
    end.

handle_cast(Msg, State) ->
    case Msg of
        {set, Value} ->
            {noreply, State#state{value = Value}};
        {set, Value, Timeout} ->
            {noreply, State#state{value = Value}, Timeout}
    end.

handle_info(timeout, State) ->
    io:format("Timeout received~n"),
    {stop, normal, State}.

terminate(_Reason, State) ->
    io:format("Key ~p timed out~n", [State#state.key]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% KV API
set(Key, Value) ->
    set_value(Key, Value, infinity).
set(Key, Value, Timeout) ->
    set_value(Key, Value, Timeout).

get(Key) ->
    get_value(Key).

set_value(Key, Value, Timeout) ->
    case is_set(Key) of
        true ->
            gen_server:cast({global, get_global(Key)}, {set, Value, Timeout});
        false ->
            {ok, _} = kv:start_link(Key, Value, Timeout)
    end,
    ok.

get_value(Key) ->
    case is_set(Key) of
        true ->
            gen_server:call({global, get_global(Key)}, get);
        false ->
            undefined
    end.

is_set(Key) ->
    Pid = global:whereis_name(get_global(Key)),
    is_pid(Pid).

get_global(Key) ->
    {kv, Key}.
