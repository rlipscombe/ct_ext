-module(ct_ext_io).
-export([put_chars/2]).

%% This is mostly here so that the unit tests can intercept it. Otherwise, we'd need to write an I/O stream handler, and
%% those are complicated. And it wouldn't work anyway, because we're running in a different node. Or would it: does
%ct:run_test propogate the 'user' process? I think it might, but we need to actually implement the whole thing.
put_chars(IoDevice, IoList) ->
    get_io_dest() ! {put_chars, IoList},
    io:put_chars(IoDevice, IoList).

get_io_dest() ->
    [_, Host] = string:split(atom_to_list(node()), "@"),
    DestNode = list_to_atom("ct_ext_io@" ++ Host),
    true = net_kernel:connect_node(DestNode),
    {ct_ext_io, DestNode}.
