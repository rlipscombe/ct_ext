-module(ct_ext_format).
-export([
    format_error/1,
    format_stacktrace/2,
    format_exception/1
]).

format_error(Error) ->
    % TODO: Truncate this? Borrow some code from lager?
    io_lib:format("~p", [Error]).

format_stacktrace(_Error, []) ->
    [];
format_stacktrace(_Error, [{Mod, _, _, _} | _]) when Mod == test_server; Mod == eunit_test ->
    [];
format_stacktrace(Error, Stack = [Frame = {M, _F, _A, Info} | Frames]) ->
    [
        format_stackframe(Frame),
        format_error_info(M, proplists:get_value(error_info, Info), Error, Stack),
        format_stacktrace(Error, Frames)
    ].

format_exception({Error, Frame}) when is_tuple(Frame) ->
    [
        ct_ext_format:format_error(Error),
        eol(),
        ct_ext_format:format_stacktrace(Error, [Frame])
    ].

format_stackframe({M, F, Args, Props}) when is_list(Args) ->
    [
        % Output the usual foo:bar/2.
        format_stackframe({M, F, length(Args), Props}),
        % If we've got the actual args, output that as well.
        "      called as ",
        io_lib:format(
            "~s:~s(",
            [M, F]
        ),
        lists:join(", ", lists:map(fun(Arg) -> io_lib:format("~p", [Arg]) end, Args)),
        ")",
        eol()
    ];
format_stackframe({M, F, Arity, Info}) when is_integer(Arity) ->
    File = proplists:get_value(file, Info),
    Line = proplists:get_value(line, Info),
    ["      at ", format_mfa(M, F, Arity, File, Line), eol()].

format_mfa(M, F, A, undefined, undefined) ->
    io_lib:format("~s:~s/~B", [M, F, A]);
format_mfa(M, F, A, File, Line) ->
    % It turns out that clicking on the location in VS Code's terminal window will take you to the correct file *and*
    % line number.
    io_lib:format("~s:~s/~B (~s, line ~B)", [M, F, A, File, Line]).

format_error_info(_Module, undefined, _Error, _Stack) ->
    [];
format_error_info(Module, ErrorInfo, Error, Stack) ->
    % TODO: This needs formatting better, but it's kinda complex (see erl_error:format_arg_errors).
    % This'll do for now.
    case get_extended_error(Module, ErrorInfo, Error, Stack) of
        ErrorMap when is_map(ErrorMap), map_size(ErrorMap) > 0 ->
            [
                io_lib:format("         ~p", [ErrorMap]),
                eol()
            ];
        _ ->
            []
    end.

get_extended_error(Module, ErrorInfo, Error, Stack) ->
    FormatModule = maps:get(module, ErrorInfo, Module),
    FormatFunction = maps:get(function, ErrorInfo, format_error),
    try
        FormatModule:FormatFunction(Error, Stack)
    catch
        error:_ ->
            #{}
    end.

eol() ->
    ct_ext_color:eol().
