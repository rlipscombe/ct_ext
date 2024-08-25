-module(failing_assert_SUITE).
-export([
    all/0,
    assert_false/1,
    assert_equal_fail/1
]).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        assert_false,
        assert_equal_fail
    ].

assert_false(_Config) -> ?assert(false).

assert_equal_fail(_Config) -> ?assertEqual(3, 4).
