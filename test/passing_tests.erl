-module(passing_tests).
-include_lib("eunit/include/eunit.hrl").

passing_test() ->
    % This runs the tests in a separate node, which makes it tricky to capture the output.
    % To get around this, we use another hook, which gets loaded into that node first, and we can use that to subvert
    % the output.

    ?assertEqual(
        {1, 0, {0, 0}},
        ct:run_test(
            [
                {dir, "test"},
                {suite, "passing_SUITE"},
                {logdir, "logs"},
                {ct_hooks, [ct_ext_test_hook, ct_ext_debug, ct_ext_summary]}
            ]
        )
    ),
    ok.
