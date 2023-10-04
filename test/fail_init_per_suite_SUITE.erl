-module(fail_init_per_suite_SUITE).
-export([all/0, init_per_suite/2, not_run/1]).

all() -> [not_run].

init_per_suite(_TestCase, _Config) -> exit(deliberately).

not_run(_) -> exit(unexpected).
