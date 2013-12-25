%%% vim: ts=4 sts=4 sw=4 expandtab

-module(corman_test_app_sup).
-behaviour(supervisor).

-export([
    start_link/0, 
    start_link_ex/3, 
    child_specs/1, 
    new_spec_for_c1/0,
    children_specs/1
]).

-define(SUP_OFO, {one_for_one, 5, 100}).
-define(SUP_SOFO, {simple_one_for_one, 5, 100}).

-export([init/1]).

%% ==================================
%% API
%% ==================================

start_link() ->
    start_link_ex(?MODULE, ?SUP_OFO, default).

start_link_ex(Name, RestartConfig, SpecMode) ->
    supervisor:start_link({local, Name}, ?MODULE, [RestartConfig, SpecMode]).

%% Returns the initial child specification for the main supervisor
child_specs(default) -> [
    spec_worker(c0),
    spec_worker(c1),
    spec_worker(c2),
    spec_sup(csup1, child_sup, ?SUP_OFO),
    spec_sup(scup2, child_simple_sup, ?SUP_SOFO)
];

%% Returns the new child specification for 
%% the main supervisor, as if we changed and released it. 
child_specs(changed) -> [
    spec_changed_worker(c1, c11),
    spec_worker(c2),
    spec_new_worker(c3),
    spec_sup(csup1, child_sup, ?SUP_OFO),
    spec_sup(scup2, child_simple_sup, ?SUP_SOFO)
];

%% Returns the initilial specification for 
%% the child supervisors
child_specs(child_sup) -> [
    spec_worker(cs1)
];

child_specs(child_simple_sup) -> [
    spec_worker(cs2)
].

%% Used to restart test
new_spec_for_c1() -> [
    spec_changed_worker(c1, c11)
].

%%% =================================
%%% Superman callbacks
%%% =================================

%% Returns the new specification for the 
%% child one_for_one supervisor, as if
%% we changed and released it.
%% Not called for the simple_one_for_one supervisor.
children_specs({_M, _F, [child_sup, _, _]}) -> [
    spec_changed_worker(cs1, cs11)
].

%% ==================================
%% Supervisor callbacks
%% ==================================

init([SpecMode]) ->
    init([?SUP_OFO, SpecMode]); 
init([RestartConfig, SpecMode]) ->
    {ok, {RestartConfig, child_specs(SpecMode)}}.

%%% ================================
%%% Internal functions
%%% ================================

spec_worker(Id) ->
    spec_worker(Id, Id, old).

spec_new_worker(Id) ->
    spec_worker(Id, Id, new).

spec_changed_worker(Id, Name) ->
    spec_worker(Id, Name, changed).

spec_worker(Id, Name, State) ->
    {Id,
        {corman_test_server, start_link, [State, Name]},
        permanent, 1000, worker, [corman_test_server]}.

spec_sup(Id, Name, RestartConfigs) ->
    {Id, {?MODULE, start_link_ex, [Name, RestartConfigs, Name]},
        permanent, 1000, supervisor, [corman_test_server]}.
