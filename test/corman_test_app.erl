-module(corman_test_app).
-behaviour(application).

-export([start/2, stop/1, set_flag/0, get_flag/0, get_configs/0]).

start(normal, _Args) ->
    corman_test_app_sup:start_link().

stop(_State) ->
    ok.

set_flag() ->
    corman_test_server:set_flag(c0, true).

get_flag() ->
    corman_test_server:get_flag(c0).

get_configs() ->
    AdditionalKeys = [included_applications],
    F = fun ({K, _V}) -> not lists:member(K, AdditionalKeys) end,
    
    Env = application:get_all_env(corman_test_app),
    Config = lists:filter(F, Env),
    lists:keysort(1, Config).
