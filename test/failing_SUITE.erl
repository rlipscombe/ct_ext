-module(failing_SUITE).
-export([
    all/0,
    fail/1
]).

-include_lib("eunit/include/eunit.hrl").

all() -> [fail].

fail(_Config) -> ?assert(false).
