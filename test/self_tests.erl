-module(self_tests).

%% Use eunit to use Common Test to test ct_ext :)

-include_lib("eunit/include/eunit.hrl").

a_test() ->
    % ct:run_test runs in a separate node, so we have to jump through some hoops to capture the output:
    {ok, _} = net_kernel:start([ct_ext_io, shortnames]),
    true = register(ct_ext_io, self()),

    _ =
        ct:run_test([
            {dir, "test"},
            {suite, passing_SUITE},
            {logdir, "logs"}
        ]),

    moo = flush([]),
    ok.

flush(Acc) ->
    receive
        M ->
            flush([M | Acc])
    after 1_500 ->
        lists:reverse(Acc)
    end.
