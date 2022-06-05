%%%-------------------------------------------------------------------
%% @doc cache public API
%% @end
%%%-------------------------------------------------------------------

-module(cache_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cache_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
