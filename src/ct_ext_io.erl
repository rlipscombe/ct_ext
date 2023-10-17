-module(ct_ext_io).
-export([put_chars/1]).

put_chars(Data) ->
    case whereis(ct_ext_io) of
        Pid when is_pid(Pid) ->
            Pid ! {put_chars, Data};
        _ ->
            io:put_chars(user, Data)
    end.
