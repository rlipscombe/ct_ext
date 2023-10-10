-module(self_tests).

%% Use eunit to use Common Test to test ct_ext :)

-include_lib("eunit/include/eunit.hrl").

a_test() ->
    ?assert(false).
