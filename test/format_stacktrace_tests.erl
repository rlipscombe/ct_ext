-module(format_stacktrace_tests).
-include_lib("eunit/include/eunit.hrl").

format_stacktrace_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun shorter/0,
        fun longer/0,
        fun error_info/0
    ]}.

setup() ->
    NoColor = os:getenv("NO_COLOR"),
    os:putenv("NO_COLOR", "1"),
    NoColor.

cleanup(false) ->
    os:unsetenv("NO_COLOR");
cleanup(NoColor) ->
    os:putenv("NO_COLOR", NoColor).

shorter() ->
    {Error, Stack} =
        {badarg, [
            {module, function,
                [
                    arg1,
                    arg2
                ],
                [
                    {file, "/path/to/module.erl"},
                    {line, 42}
                ]}
        ]},
    Expected =
        <<
            "      at module:function/2 (/path/to/module.erl, line 42)\n"
            "      called as module:function(arg1, arg2)\r\n"
        >>,
    ?assertEqual(Expected, iolist_to_binary(ct_ext_format:format_stacktrace(Error, Stack))),
    ok.

longer() ->
    % Captured from one of the 'eventually' unit tests; can we format it nicely?
    {Error, Stack} =
        {eventually_assert_failed, [
            {eventually, assert_error,
                [
                    {probe, always_fail, '#Fun<eventually.7.121276098>', false},
                    {matcher, never_match, '#Fun<description_tests.2.56923557>'}
                ],
                [
                    {file, "/path/to/eventually/src/eventually.erl"},
                    {line, 156},
                    {error_info, #{module => eventually}}
                ]},
            {description_tests, both_description_test, 0, [
                {file, "/path/to/eventually/test/description_tests.erl"},
                {line, 12}
            ]},
            {eunit_test, '-mf_wrapper/2-fun-0-', 2, [{file, "eunit_test.erl"}, {line, 273}]},
            {eunit_test, run_testfun, 1, [{file, "eunit_test.erl"}, {line, 71}]},
            {eunit_proc, run_test, 1, [{file, "eunit_proc.erl"}, {line, 531}]},
            {eunit_proc, with_timeout, 3, [{file, "eunit_proc.erl"}, {line, 356}]},
            {eunit_proc, handle_test, 2, [{file, "eunit_proc.erl"}, {line, 514}]},
            {eunit_proc, tests_inorder, 3, [{file, "eunit_proc.erl"}, {line, 456}]}
        ]},

    % TODO: We output \r\n for 'called as', but not for 'at'. Consistency: we've heard of it.
    Expected =
        <<
            "      at eventually:assert_error/2 (/path/to/eventually/src/eventually.erl, line 156)\n"
            "      called as eventually:assert_error({probe,always_fail,'#Fun<eventually.7.121276098>',false}, {matcher,never_match,'#Fun<description_tests.2.56923557>'})\r\n"
            "      at description_tests:both_description_test/0 (/path/to/eventually/test/description_tests.erl, line 12)\n"
        >>,
    ?assertEqual(Expected, iolist_to_binary(ct_ext_format:format_stacktrace(Error, Stack))),
    ok.

error_info() ->
    {'EXIT', {Error, Stack}} = catch maps:map(not_a_fun, not_a_map),
    Expected = <<
        "      at maps:map/2 (maps.erl, line 608)\n"
        "      called as maps:map(not_a_fun, not_a_map)\r\n"
        "         #{1 => <<\"not a fun that takes two arguments\">>,\n"
        "           2 => <<\"not a map or an iterator\">>}\r\n"
        "      at format_stacktrace_tests:error_info/0 (test/format_stacktrace_tests.erl, line 79)\n"
    >>,
    ?assertEqual(Expected, iolist_to_binary(ct_ext_format:format_stacktrace(Error, Stack))),
    ok.
