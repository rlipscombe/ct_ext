-module(passing_SUITE).
-export([
    all/0,
    pass/1
]).

all() -> [pass].

pass(_Config) -> ok.
