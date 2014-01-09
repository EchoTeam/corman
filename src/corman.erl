-module(corman).

%% API
-export([reload/0,
         reload/1,
         reload/2,
         reload/3]).


-type application() :: atom().

%%%===================================================================
%%% API
%%%===================================================================
-spec reload() -> {'ok', [application()]}.
reload() ->
    reload([]).

-spec reload(AppsToRestart :: [application()]) -> {'ok', [application()]}.
reload(AppsToRestart) ->
    AvailableApplications = [Application || {Application, _, _} <- application:loaded_applications()],
    reload(AvailableApplications, AppsToRestart).

-spec reload(Applications :: [application()], AppsToRestart :: [application()]) -> {'ok', [application()]}.
reload(Applications, AppsToRestart) ->
    {ok, [[File]]} = init:get_argument(config),
    reload(Applications, AppsToRestart, File).

-spec reload(Applications:: [application()], AppsToRestart :: [application()], ConfigFile :: file:name_all()) -> {'ok', [application()]}.
reload(Applications, AppsToRestart, ConfigFile) ->
    {ok, Config} = check_config(ConfigFile),
    reload_ll(Applications, Config, AppsToRestart).

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
check_config(File) ->
    % Sometimes File can be without ".config" extension,
    % so we normalize the file name.
    Dirname = filename:dirname(File),
    Basename = filename:basename(File, ".config"),
    RealFileName = filename:join(Dirname, Basename) ++ ".config",
    {ok, Env} = parse_config(RealFileName),
    Files = [Term || Term <- Env, is_list(Term)],
    case check_config(Files, []) of
        ok -> {ok, Env};
        E -> E
    end.

check_config([], []) ->
    ok;

check_config([], Errors) ->
    {error, lists:reverse(Errors)};

check_config([File | Files], Errors) ->
    case parse_config(File) of
        {ok, _} -> check_config(Files, Errors);
        E -> check_config(Files, [E | Errors])
    end.

parse_config(File) ->
    case file:consult(File) of
        {ok, [Data]} -> {ok, Data};
        {error, E} -> {error, File, E}
    end.


reload_ll(Applications, Config, AppsToRestart) ->
    case application_specs(Applications) of
        {incorrect_specs, IncorrectApps} ->
            lager:error("Unable to reload applications configs.~n The following applications have incorrect specifications ~p", [IncorrectApps]),
            {error, {incorrect_specs, IncorrectApps}};
        Specs ->
            {change_application_data(Specs, Config, AppsToRestart), Applications}
    end.


application_specs(Applications) ->
    Specs = [{application, Application, make_application_spec(Application)} || Application <- Applications],
    IncorrectApplications = [Application || {_, Application, incorrect_spec} <- Specs],
    case IncorrectApplications of
        [] -> Specs;
        _ -> {incorrect_specs, IncorrectApplications}
    end.

make_application_spec(Application) when is_atom(Application) ->
    {ok, LoadedAppSpec} = application:get_all_key(Application),
    case code:where_is_file(atom_to_list(Application) ++ ".app") of
        non_existing ->
            LoadedAppSpec;
        AppSpecPath when is_list(AppSpecPath) ->
            make_application_spec(LoadedAppSpec, AppSpecPath)
    end.

make_application_spec(LoadedAppSpec, AppSpecPath) ->
    case file:consult(AppSpecPath) of
        {ok, [{application, _, AppSpec}]} ->
            ParamsToUpdate = [env, modules, included_applications],
            lists:foldl(fun(P, Acc) ->
                V = proplists:get_value(P, AppSpec, []),
                lists:keyreplace(P, 1, Acc, {P, V})
            end, LoadedAppSpec, ParamsToUpdate);
        {error, _Reason} ->
            incorrect_spec
    end.


change_application_data(Specs, Config, AppsToRestart) ->
    lager:info("Update configurations for the following applications: ~p", [[App || {_, App, _} <- Specs]]),
    % Fetch OLD applications' environment from
    % application controller's internal ETS table.
    OldEnv = application_controller:prep_config_change(),

    % Stores the environment to the application controller's ETS table.
    ok = application_controller:change_application_data(Specs, Config),

    % Notify running applications (via config_change/3 callback) about
    % configuration changes derived from comparing EnvBefore with the
    % content of the application controller's ETS table storing
    % applications' environment.
    application_controller:config_change(OldEnv),

    % Restart apps from the list if their configs have been changed
    maybe_restart_apps(AppsToRestart, OldEnv).

%% Restart particular app in Apps list if the OldEnv is different from the NewEnv
maybe_restart_apps(Applications, OldEnv) ->
    lists:foreach(
        fun(Application) ->
            CurrentEnv = application:get_all_env(Application),
            PreviousEnv = proplists:get_value(Application, OldEnv, []),
            case CurrentEnv of
                PreviousEnv ->
                    ok;
                _ ->
                    StartedApps = proplists:get_value(started, application_controller:info()),
                    ApplicationStartType = proplists:get_value(Application, StartedApps),
                    application:stop(Application),
                    application:start(Application, ApplicationStartType)
            end
        end,
    Applications).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

check_config_test() ->
    ?assertMatch({ok, _}, check_config(data("corman-2.config"))),
    ?assertMatch({error, _}, check_config(data("corman-3.config"))),
    ?assertMatch({ok, _}, check_config(data("corman"))),
    ok.

make_application_spec_test() ->
    Spec = [{env, [
                {k1, v1}, 
                {k2, v2}
            ]}, 
            {modules, [m1, m2]}
           ], 

    Spec1 = make_application_spec(Spec, data("corman-1.app.config")),
    Env1 = proplists:get_value(env, Spec1),
    ?assertEqual(v11, proplists:get_value(k1, Env1)),
    ?assertEqual(v3, proplists:get_value(k3, Env1)),
    ?assertNot(proplists:is_defined(k2, Env1)),
    Modules = proplists:get_value(modules, Spec1),
    ?assertMatch([m1, m3], Modules),

    Spec2 = make_application_spec(Spec, data("corman-2.app.config")),
    ?assertEqual([], proplists:get_value(env, Spec2)),
    ?assertEqual([], proplists:get_value(modules, Spec2)),

    ?assertEqual(incorrect_spec, make_application_spec(Spec, data("corman-3.app.config"))),
    ok.

data(Name) ->
    filename:join("../test/data", Name).

-endif.
