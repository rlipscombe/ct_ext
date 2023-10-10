-module(ct_ext_io).
-export([put_chars/2]).

%% This is mostly here so that the unit tests can intercept it with meck. Otherwise, we'd need to write an I/O stream
%% handler, and that would be hard.
put_chars(IoDevice, IoList) ->
    io:put_chars(IoDevice, IoList).
