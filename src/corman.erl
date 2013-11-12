-module(corman).

%% API
-export([reload/1,
         reload/2]).


-type application() :: atom().

%%%===================================================================
%%% API
%%%===================================================================

-spec reload(AppsToRestart :: [application()]) -> {'ok', [application()]}.
reload(AppsToRestart) ->
    AvailableApplications = [Application || {Application, _, _} <- application:loaded_applications()],

    reload_ll(AvailableApplications, AppsToRestart).

-spec reload(Applications :: [application()], AppsToRestart :: [application()]) -> {'ok', [application()]}.
reload(Applications, AppsToRestart) ->
    reload_ll(Applications, AppsToRestart).

%%%===================================================================
%%% Internal functions
%%%===================================================================

reload_ll(Applications, AppsToRestart) ->
    {ok, [[File]]} = init:get_argument(config),
    {ok, Config} = check_config(File),
    reload_ll(Applications, Config, AppsToRestart).


check_config(File) ->
    {ok, Env} = parse_config(File),
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
            Env = proplists:get_value(env, AppSpec, []),
            lists:keyreplace(env, 1, LoadedAppSpec, {env, Env});
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

