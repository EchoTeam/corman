-module(corman_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_with_restarting/1,
    test_without_restarting/1
]).

-define(CHANGED_CONFIG, "test/data/test_app_new.config").
-define(INITIAL_CONFIG, "test/data/test_app.config").

all() -> [
    test_without_restarting, 
    test_with_restarting     
].

init_per_testcase(_, Config) ->
    load_initial_config(),
    ok = application:load(corman_test_app_spec()),
    ok = application:start(corman_test_app),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(corman_test_app),
    ok = application:unload(corman_test_app).

%% =====================================
%% Tests
%% =====================================

test_without_restarting(_Config) ->
    test_reloading(false, [corman_test_app]).

test_with_restarting(_Config) ->
    test_reloading(true, []).

%% =====================================
%% Internal functions
%% =====================================

%% Checks the configs before and after reloading.
%% Checks the flag after restarting. 
test_reloading(Flag, AppsToRestart) ->
    ok = corman_test_app:set_flag(),
    ConfigBefore = config_before(),
    ConfigAfter = config_after(),
    ChangedConfigPath = config(?CHANGED_CONFIG),

    ConfigBefore = corman_test_app:get_configs(),
    {ok, [corman_test_app]} = corman:reload([corman_test_app], AppsToRestart, ChangedConfigPath),
    ConfigAfter = corman_test_app:get_configs(),
    Flag = corman_test_app:get_flag().
    
%% Returns the initial spec for the test application
corman_test_app_spec() ->
    {application, corman_test_app, [
        {mod, {corman_test_app, []}},
        {modules, [corman_test_app, corman_test_app_sup, corman_test_server]},
        {env, [
            {config1, old},
            {config2, old},
            {config3, old}
        ]}
    ]}.

config_before() -> [
    {config1, old},
    {config2, old},
    {config3, old},
    {config_param1, old},
    {config_param2, old},
    {config_param3, old}
].

config_after() -> [
    {config1, old},
    {config2, changed},
    {config4, new},
    {config_param1, changed},
    {config_param2, old},
    {config_param4, new}
].

load_initial_config() ->
    ConfigPath = config(?INITIAL_CONFIG),
    {ok, [Config]} = file:consult(ConfigPath),
    ok = application_controller:change_application_data([], Config).

config(Config) ->
    %% The cwd is the common test run session (logs/ct_run...)
    %% double up and we are in the corman/ 
    {ok, Cwd} = file:get_cwd(),
    F = fun(Path) -> filename:dirname(Path) end,
    CormanRoot = F(F(Cwd)),
    filename:join(CormanRoot, Config).
