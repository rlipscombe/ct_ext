-module(passing_tests).
-include_lib("eunit/include/eunit.hrl").

passing_test() ->
    % This runs the tests in a separate node, which makes it tricky to capture the output.
    % To get around this, we use another hook, which gets loaded into that node first, and we can use that to subvert
    % the output.

    ?assertEqual({1, 0, {0, 0}},
        ct:run_test(
            [{dir, "test"},
            {suite, "passing_SUITE"},
        {logdir, "logs"}])),
    ok.
    % % Ideally, I'd like to just capture the output from the hook, and run it with actual suites.
    % % Unfortunately, that's really difficult: the CT tests run in a separate node
    % Hook = ct_ext_summary,
    % Opts = [],

    % Id = Hook:id(Opts),
    % {ok, State} = Hook:init(Id, Opts),

    % % TODO: post_groups
    % % TODO: post_all

    % {Config, State2} = Hook:pre_init_per_suite(SuiteName, InitData, State),
    % ok.
