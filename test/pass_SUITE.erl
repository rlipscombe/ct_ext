-module(pass_SUITE).
-export([all/0, pass/1]).
-export([pass_/1]).

-include("ct_ext_test.hrl").

all() -> [pass].

pass(_Config) ->
    _ = ?RUN_TEST(?MODULE, pass_, [ct_ext_summary]),
    ok.

pass_(_Config) ->
    ok.
