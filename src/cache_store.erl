-module(cache_store).
%%---------------------------------------------------------------------%%
-behavior(gen_server).
%%---------------------------------------------------------------------%%
-export([start_link/3]).
-export([get/2]).
%%---------------------------------------------------------------------%%
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
%%---------------------------------------------------------------------%%
-define(CACHE, cache).
%%---------------------------------------------------------------------%%
-type key() :: atom().
-type refresh_interval() :: non_neg_integer().
-type time_out() :: non_neg_integer().
-type result() :: {ok, any()} | {error, timeout()}.
%%---------------------------------------------------------------------%%
-record(state, { key :: key(),
                 func :: fun(),
                 refresh_interval :: refresh_interval() }).
%%---------------------------------------------------------------------%%
-spec start_link(atom(), fun(), refresh_interval()) -> {ok, pid()}.
start_link(Name, Fun, RefreshInterval) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Fun, RefreshInterval], []).

-spec get(key(), time_out()) -> result().
get(Name, Timeout) ->
    try
        gen_server:call(Name, get, Timeout)
    catch exit:{timeout, _Data} -> {error, timeout}
    end.

%%---------------------------------------------------------------------%%
init([Key, Fun, RefreshInterval]) ->
    process_flag(trap_exit, true),
    erlang:send_after(RefreshInterval, self(), refresh),
    {ok, #state{ key=Key,
                 func=Fun,
                 refresh_interval=RefreshInterval }}.

handle_call(get, _From, State) ->
    Fun = State#state.func,
    RefreshInterval = State#state.refresh_interval,
    erlang:send_after(RefreshInterval, self(), refresh),
    {reply, {ok, Fun()}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State=#state{ key=Key, func=Fun }) ->
    [{Key, _Value, TTL, RefreshInterval, _Then}] = ets:lookup(?CACHE, Key),
    NewValue = Fun(),
    Now = cache_utils:get_current_time_in_millisec(),
    ets:insert(?CACHE, {Key, NewValue, TTL, RefreshInterval, Now}),
    erlang:send_after(RefreshInterval, self(), refresh),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


