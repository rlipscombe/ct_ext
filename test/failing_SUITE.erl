-module(failing_SUITE).
-export([
    all/0,
    throw_error/1,
    call_stack/1
]).

all() ->
    [
        throw_error,
        call_stack
    ].

throw_error(_Config) -> error(computer_says_no).

call_stack(_Config) -> recursive_call(5).

recursive_call(N) when N > 0 ->
    % It'll throw a function_clause error; that's deliberate.
    recursive_call(N - 1).
