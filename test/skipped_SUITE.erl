-module(skipped_SUITE).
-export([
    all/0,
    not_run/1,
    init_per_testcase/2
]).

all() -> [not_run].

init_per_testcase(_TestCase, _Config) ->
    {skip, reason_to_skip}.

not_run(_Config) -> exit(unexpected).

% TODO: all lists a non-existent test.
% TODO: init fails, rather than skips.
