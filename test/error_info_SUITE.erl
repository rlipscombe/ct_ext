-module(error_info_SUITE).
-export([
    all/0,
    add_atoms/1,
    parse_hello/1,
    div_by_zero/1
]).

%% We should format the error_info properly.

all() ->
    [
        add_atoms,
        parse_hello,
        div_by_zero
    ].

add_atoms(_Config) ->
    % 1> erlang:'+'(foo, bar).
    % ** exception error: an error occurred when evaluating an arithmetic expression
    %      in operator  +/2
    %         called as foo + bar
    erlang:'+'(foo, bar).

parse_hello(_Config) ->
    % 2> erl_parse:parse("hello.").
    % ** exception error: bad argument
    %      in function  element/2
    %         called as element(1,104)
    %         *** argument 2: not a tuple
    %      in call from erl_parse:yeccpars1/5 (/Users/rogerlipscombe/.kerl/builds/OTP-25.3.2.5/otp_src_git/bootstrap/lib/parsetools/include/yeccpre.hrl, line 83)
    %      in call from erl_parse:yeccpars0/5 (/Users/rogerlipscombe/.kerl/builds/OTP-25.3.2.5/otp_src_git/bootstrap/lib/parsetools/include/yeccpre.hrl, line 57)
    erl_parse:parse("hello.").

div_by_zero(_Config) ->
    _ = 9 div 0.
