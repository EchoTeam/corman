%%% vim: ts=4 sts=4 sw=4 expandtab

-module(superman_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    reconfigure_supervisor_test/1,
    restart_children_test/1,
    start_children_test/1,
    get_child_specs_test/1,
    reconfigure_tree_test/1
]).

-define(TEST_SUP, corman_test_app_sup).
-define(TEST_SIMPLE_SUP, child_simple_sup).

all() -> [
    reconfigure_supervisor_test,
    restart_children_test,
    start_children_test,
    get_child_specs_test,
    reconfigure_tree_test
].

init_per_testcase(_, Config) ->
    {ok, Pid} = corman_test_app_sup:start_link(),
    set_flags([c0, c1, c2, cs1]),
    Config.

end_per_testcase(_, _) ->
    ok.

%%% =======================================
%%% Tests
%%% =======================================

reconfigure_supervisor_test(_Config) ->
    Spec = corman_test_app_sup:child_specs(changed),
    ok = superman:reconfigure_supervisor(?TEST_SUP, Spec),
    
    States = [{c0, undefined},
              {c1, undefined},
              {c11, changed, false},
              {c2, old, true},
              {c3, new, false}],

    check_states(States). 

restart_children_test(_Config) ->
    Spec = corman_test_app_sup:new_spec_for_c1(),
    ok = superman:restart_children(?TEST_SUP, Spec),
    States = [{c0, old, true},
              {c1, undefined},
              {c11, changed, false},
              {c2, old, true}],

    check_states(States).

start_children_test(_Config) ->
    ok = supervisor:terminate_child(?TEST_SUP, c2),

    %% c2 is already_present
    %% others are already_started
    [{c3, {started, _}}] = superman:start_children(?TEST_SUP, [changed]),
    ok.

get_child_specs_test(_Config) ->
    F = fun(SpecMode) ->
            V1 = corman_test_app_sup:child_specs(SpecMode),  
            V2 = superman:get_child_specs(?TEST_SUP, [SpecMode]),
            V1 = V2,
            ok
         end,
    ok = F(changed),
    ok = F(default).

reconfigure_tree_test(_Config) ->
    %% Add only one worker to the simple_one_for_one supervisor,
    %% because we must check its state by {local, cs2}.
    supervisor:start_child(?TEST_SIMPLE_SUP, []),
    set_flags([cs2]),

    Spec = corman_test_app_sup:child_specs(changed),
    ok = superman:reconfigure_supervisor_tree(?TEST_SUP, Spec),

    States = [{c0, undefined},
              {c1, undefined},
              {c11, changed, false},
              {c2, old, true},
              {c3, new, false},
              {cs1, undefined},
              {cs11, changed, false},
              {cs2, old, true}],

    ok = check_states(States).

%%% =======================================
%%% Internal functions
%%% =======================================

check_states(Results) ->
    lists:foreach(fun(R) -> check_state(R) end, Results).
    
check_state({Name, Params, Flag}) ->
    Params = corman_test_server:get_state(Name),
    Flag = corman_test_server:get_flag(Name);

check_state({Name, undefined}) ->
    undefined = whereis(Name).

set_flags(Names) ->
    lists:foreach(fun(Name) -> corman_test_server:set_flag(Name, true) end, Names).

