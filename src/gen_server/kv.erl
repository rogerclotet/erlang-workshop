-module(kv).

-behaviour(gen_server).

%% Start server
-export([start_link/1]).

%% Server API
-export([set/2, set/3, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {key, value, expire}).

%% GEN_SERVER (kv processes)
start_link([Key, Value, Expire]) ->
    gen_server:start_link({global, get_global(Key)}, ?MODULE, [Key, Value, Expire], []).

init([Key, Value, Expire]) ->
    {ok, #state{key = Key, value = Value, expire = Expire}, 0}.

handle_call(get, _From, State) ->
    {reply, State#state.value, State}.

handle_cast(Msg, State) ->
    case Msg of
        {set, Value} ->
            {noreply, State#state{value = Value}};
        {set, Value, Expire} ->
            timer:send_after(Expire, expire),
            {noreply, State#state{value = Value}}
    end.

handle_info(timeout, State) ->
    case State#state.expire of
        infinity -> {noreply, State};
        Expire ->
            Ref = timer:send_after(Expire, expire),
            {noreply, State#state{expire = Ref}}
    end;
handle_info(expire, State) ->
    {stop, normal, State}.

terminate(_Reason, State) ->
    io:format("Key ~p expired~n", [State#state.key]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% KV API
set(Key, Value) ->
    set_value(Key, Value, infinity).
set(Key, Value, Expire) ->
    set_value(Key, Value, Expire).

get(Key) ->
    get_value(Key).

set_value(Key, Value, Expire) ->
    case global:whereis_name({kv, Key}) of
        undefined ->
            {ok, _} = if
                Expire =:= infinity -> kv:start_link([Key, Value]);
                true -> kv:start_link([Key, Value, Expire])
            end;
        Pid ->
            Message = if
                Expire =:= infinity -> {set, Value};
                true -> {set, Value, Expire}
            end,
            gen_server:cast(Pid, Message)
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
