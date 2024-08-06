-module(self_test_SUITE).
-export([
    all/0,
    experiment/1
]).

-include_lib("common_test/include/ct.hrl").

all() -> [experiment].

experiment(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    % We can't run ct:run_test in a node that's already running CT, so we need another node.
    ct:capture_start(),
    PaDir = filename:join(code:lib_dir(ct_ext), "ebin"),
    {ok, Peer, NodeName} = ?CT_PEER(["-pa", PaDir]),
    % Because we're run by erlang.mk's 'ct' task, we're running in logs/ct_run.whatever,
    % and so is our peer. This means that any paths we give are relative to that (or should be absolute).
    Opts = [
        {dir, DataDir},
        {ct_hooks, [ct_ext_summary]}
        %    {logdir, "logs"}
    ],
    {_Ok, _Failed, {_UserSkipped, _AutoSkipped}} = rpc:call(NodeName, ct, run_test, [Opts]),
    peer:stop(Peer),
    ct:capture_stop(),
    moo = ct:capture_get(),
    ok.
