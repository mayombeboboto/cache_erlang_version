-module(cache_server).
%%---------------------------------------------------------------------%%
-behavior(gen_server).
%%---------------------------------------------------------------------%%
-export([start_link/0]).
-export([stop/0]).
%%---------------------------------------------------------------------%%
-export([register_function/4]).
-export([get/3]).
%%---------------------------------------------------------------------%%
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
%%---------------------------------------------------------------------%%
-define(CACHE, cache).
%%---------------------------------------------------------------------%%
-type result() :: {ok, any()} |
                  {error, timeout} |
                  {error, not_registered}.

-type key() :: atom().
-type ttl() :: non_neg_integer().
-type refresh_interval() :: non_neg_integer().
-type time_out() :: non_neg_integer().
-type options() :: list().
%%---------------------------------------------------------------------%%
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> no_return().
stop() ->
    gen_server:cast(?MODULE, stop).

-spec register_function(fun(), key(), ttl(), refresh_interval()) -> ok | {error, already_registered}.
register_function(Fun, Key, TTL, RefreshInterval) when is_function(Fun, 0) andalso
                                                       is_atom(Key) andalso
                                                       is_integer(TTL) andalso
                                                       is_integer(RefreshInterval) andalso
                                                       TTL > 0 andalso
                                                       RefreshInterval > 0 ->
    gen_server:call(?MODULE, {register_fun, Fun, Key, TTL, RefreshInterval}).

-spec get(key(), time_out(), options()) -> result().
get(Key, Timeout, _Options) when is_atom(Key) andalso
                                 is_integer(Timeout) andalso
                                 Timeout > 0 ->
    gen_server:call(?MODULE, {get, Key, Timeout}).

%%---------------------------------------------------------------------%%
init([]) ->
    ets:new(?CACHE, [named_table, public, {keypos, 1}]),
    {ok, []}.

handle_call({register_fun, Fun, Key, TTL, RefreshInterval}, _From, State) ->
    case ets:lookup(?CACHE, Key) of
        [] ->
            Now = cache_utils:get_current_time_in_millisec(),
            ets:insert(?CACHE, {Key, Fun(), TTL, RefreshInterval, Now}),
            cache_store:start_link(Key, Fun, RefreshInterval),
            {reply, ok, [Key|State]};
        [_Value] -> {reply, {error, already_registered},  State}
    end;
handle_call({get, Key, Timeout}, _From, State) ->
    case ets:lookup(?CACHE, Key) of
        [] -> {reply, {error, not_registered}, State};
        [{Key, Value, TTL, _RefreshInterval, Then}] ->
            Reply = get_value(Key, Value, TTL, Then, Timeout),
            {reply, Reply, State}
    end.

handle_cast(stop, State) ->
    {stop, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    [exit(Key) || Key <- State].

%%---------------------------------------------------------------------%%
get_value(Key, Value, TTL, Then, Timeout) ->
    Now = cache_utils:get_current_time_in_millisec(),
    if Now-Then < TTL -> {ok, Value};
       true -> cache_store:get(Key, Timeout)
    end.

