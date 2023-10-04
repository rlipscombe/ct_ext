-module(fail_init_per_suite_SUITE).
-export([all/0, init_per_suite/1, not_run/1]).

all() -> [not_run].

init_per_suite(_Config) ->
    % Note that if you exit(deliberately), you don't get the stack trace.
    error(deliberately).

not_run(_) -> exit(unexpected).
