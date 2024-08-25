-module(ct_ext_test).
-export([run_test/3]).

-include_lib("common_test/include/ct.hrl").
-define(APPLICATION, ct_ext).

run_test(Suite, Case, Hooks) ->
    {ok, Peer, NodeName} = ?CT_PEER([
        "-pa",
        filename:join(code:lib_dir(?APPLICATION), "ebin"),
        "-env",
        "NO_COLOR",
        "1"
    ]),
    Dir = filename:dirname(code:which(Suite)),
    Opts = [
        {ct_hooks, Hooks},

        {dir, Dir},
        {suite, [Suite]},
        % TODO: This bit varies.
        {testcase, [Case]}
    ],
    ct:capture_start(),
    case rpc:call(NodeName, ct, run_test, [Opts]) of
        {error, Reason} ->
            ct:fail(Reason);
        {_, _, {_, _}} ->
            % TODO: The child test is sometimes allowed to fail (depends on the test).
            ok
    end,
    peer:stop(Peer),
    ct:capture_stop(),
    ct:capture_get().
