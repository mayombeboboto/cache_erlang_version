-module(cache_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([test_get_value_success/1]).
-export([test_refresh_data_success/1]).
-export([test_duplicate_registration/1]).
-export([test_get_with_unknown_key/1]).
-export([test_get_with_timeout/1]).

all() -> [
            test_get_value_success,
            test_refresh_data_success,
            test_duplicate_registration,
            test_get_with_unknown_key,
            test_get_with_timeout
        ].

init_per_testcase(_, Config) ->
    cache_server:start_link(),
    Config.

end_per_testcase(_, _Config) ->
    cache_server:stop().

test_get_value_success(_config) ->
    Fun = fun() -> calendar:local_time() end,
    ok = cache_server:register_function(Fun, my_key, 4000, 2000),
    {ok, _Datetime} = cache_server:get(my_key, 3000, []),
    ok.

test_refresh_data_success(_Config) ->
    Fun = fun() -> calendar:local_time() end,
    ok = cache_server:register_function(Fun, my_key, 4000, 1000),
    {ok, Datetime1} = cache_server:get(my_key, 3000, []),

    timer:sleep(3500),
    {ok, Datetime2} = cache_server:get(my_key, 3000, []),
    true = Datetime1 =/= Datetime2,
    ok.

test_duplicate_registration(_Config) ->
    Fun = fun() -> ok end,
    ok = cache_server:register_function(Fun, my_key, 4000, 2000),

    {error, already_registered} = cache_server:register_function(Fun, my_key, 4000, 2000),
    ok.

test_get_with_unknown_key(_Config) ->
    {error, not_registered} = cache_server:get(unknown_key, 1000, []),
    ok.

test_get_with_timeout(_Config) ->
    Fun = fun() -> timer:sleep(3500), ok end,
    ok = cache_server:register_function(Fun, my_key, 1000, 300),
    Results = [cache_server:get(my_key, 10, []) || _Value <- lists:seq(1, 1000)],

    true = lists:any(fun(Value) -> Value == {error,timeout} end, Results),
    ok.
