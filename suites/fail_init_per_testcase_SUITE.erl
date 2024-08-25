-module(fail_init_per_testcase_SUITE).
-export([all/0, init_per_testcase/2, not_run/1]).

all() -> [not_run].

init_per_testcase(_TestCase, _Config) -> exit(deliberately).

not_run(_) -> exit(unexpected).
