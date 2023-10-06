-module(failing_SUITE).
-export([
    all/0,
    throw_error/1,
    call_stack/1,
    tail_call/1
]).

all() ->
    [
        throw_error,
        call_stack,
        tail_call
    ].

throw_error(_Config) -> error(computer_says_no).

call_stack(_Config) ->
    % This nastiness is to prevent tail call optimizations, so we get a decently-deep stack.
    eval({add, 5, {add, {sub, 5, 4}, {divide, 3, {sub, 3, 3}}}}).

eval(N) when is_integer(N) -> N;
eval({add, L, R}) -> eval(L) + eval(R);
eval({sub, L, R}) -> eval(L) - eval(R);
eval({divide, L, R}) -> eval(L) div eval(R).

tail_call(_Config) -> recursive_call(5).

recursive_call(N) when N > 0 ->
    % It'll throw a function_clause error; that's deliberate.
    recursive_call(N - 1).
