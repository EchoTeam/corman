{application, corman_test_app, [
    {modules, [corman_test_app, corman_test_server, corman_test_app_sup]},
    {mod, {corman_test_app, []}},
    {env, [
        {config1, old},
        {config2, changed},
        {config4, new}
    ]}
]}.
