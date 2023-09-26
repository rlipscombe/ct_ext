-module(failing_SUITE).
-export([
    all/0,
    assert_false/1,
    call_stack/1
]).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        assert_false,
        call_stack
    ].

assert_false(_Config) -> ?assert(false).

call_stack(_Config) -> recursive_call(5).

recursive_call(N) when N > 0 ->
    % It'll throw a function_clause error; that's deliberate.
    recursive_call(N - 1).
