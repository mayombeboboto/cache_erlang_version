-module(cache_utils).
%%---------------------------------------------------------------------%%
-export([get_current_time_in_millisec/0]).
%%---------------------------------------------------------------------%%

get_current_time_in_millisec() ->
    {_Date, Time} = calendar:local_time(),
    calendar:time_to_seconds(Time) * 1000.