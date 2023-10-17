-module(ct_ext_test_hook).
-export([
    init/2,
    terminate/1
]).

init(_Id, _Opts) ->
    % We're running in the CT node, so we can hook stuff in it.
    Pid = spawn(fun capture_init/0),
    {ok, Pid}.

terminate(Pid) ->
    Pid ! terminate.

capture_init() ->
    register(ct_ext_io, self()),
    capture_loop().

capture_loop() ->
    receive
        terminate ->
            ok;
        M ->
            io:put_chars(user, io_lib:format("~p", [M])),
            file:write_file("/home/roger/Source/rlipscombe/ct_ext/io.log", io_lib:format("~p~n", [M]), [append]),
            capture_loop()
    end.
