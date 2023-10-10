-module(self_tests).

%% Use eunit to use Common Test to test ct_ext :)

-include_lib("eunit/include/eunit.hrl").

a_test() ->
    ok = meck:new(ct_ext_io, [no_link]),
    meck:expect(ct_ext_io, put_chars, fun(_, _) -> ok end),
    {_Ok, _Failed, {_UserSkipped, _AutoSkipped}} =
        ct:run_test([
            {dir, "test"},
            {suite, passing_SUITE},
            {logdir, "logs"}
        ]),
    meck:wait(ct_ext_io, put_chars, ['_', '_'], 2_500),
    % ?assertNotEqual([], meck:history(ct_ext_io)),
    ok.
