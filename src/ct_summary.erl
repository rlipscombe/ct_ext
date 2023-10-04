-module(ct_summary).
-export([
    init/2,
    pre_init_per_testcase/4,
    post_end_per_testcase/5,
    on_tc_skip/4,
    on_tc_fail/4,
    terminate/1
]).

-include("colors.hrl").
-include("glyphs.hrl").

-record(state, {
    % erlang:monotonic_time, native units.
    case_started_at,
    % list of completed test cases, with results
    cases = []
}).

-define(APPLICATION, ct_report).

init(_Id, _Opts) ->
    % Load application environment.
    application:load(?APPLICATION),

    % For some reason, Erlang doesn't output Unicode correctly when -noinput or -noshell are specified.
    % Fix that by setting the option back.
    io:setopts(standard_io, [{encoding, unicode}]),
    State = #state{},
    {ok, State}.

pre_init_per_testcase(_Suite, _TestCase, InitData, State) ->
    {InitData, State#state{case_started_at = erlang:monotonic_time()}}.

post_end_per_testcase(
    Suite,
    TestCase,
    _Config,
    Return = ok,
    State = #state{case_started_at = StartedAt, cases = Cases}
) ->
    EndedAt = erlang:monotonic_time(),
    {Return, State#state{cases = [{passed, Suite, TestCase, StartedAt, EndedAt} | Cases]}};
post_end_per_testcase(
    _Suite,
    _TestCase,
    _Config,
    Return = {error, _},
    State
) ->
    % No need; on_tc_skip or on_tc_fail will be called, and since we need on_tc_fail for init_per_suite failures, we'd
    % be better off doing it there.
    {Return, State}.

on_tc_skip(Suite, TestCase, Reason, State = #state{case_started_at = StartedAt, cases = Cases}) ->
    EndedAt = erlang:monotonic_time(),
    State#state{cases = [{skipped, Suite, TestCase, Reason, StartedAt, EndedAt} | Cases]}.

%% We use on_tc_fail rather than post_end_per_testcase, because on_tc_fail also gets notified about init_per_suite,
%% etc., failures.
on_tc_fail(Suite, TestCase, Reason, State = #state{case_started_at = StartedAt, cases = Cases}) ->
    EndedAt = erlang:monotonic_time(),
    State#state{cases = [{failed, Suite, TestCase, Reason, StartedAt, EndedAt} | Cases]}.

terminate(_State = #state{cases = Cases}) ->
    lists:foreach(fun report/1, Cases),
    io:put_chars(user, ["\e[0m", "\r\n"]).

report({passed, Suite, TestCase, StartedAt, EndedAt}) ->
    report_test_case(
        color(passed), ?TEST_PASSED_GLYPH, Suite, TestCase, " passed", StartedAt, EndedAt
    );
report({failed, ct_framework, error_in_suite, {error, Error}, _StartedAt, _EndedAt}) ->
    io:put_chars(user, [
        "  ",
        color(missing),
        ?TEST_MISSING_GLYPH,
        " ",
        io_lib:format("~s", [Error]),
        eol()
    ]);
report({failed, Suite, TestCase, Reason, StartedAt, EndedAt}) ->
    report_test_case(
        color(failed), ?TEST_FAILED_GLYPH, Suite, TestCase, " failed", StartedAt, EndedAt
    ),
    report_reason(Reason),
    ok;
report(
    {skipped, Suite, TestCase, _Reason = {tc_auto_skip, {failed, {Suite, Function, _}}}, StartedAt,
        EndedAt}
) ->
    io:put_chars(user, [
        "  ",
        color(skipped),
        ?TEST_SKIPPED_GLYPH,
        " ",
        io_lib:format("~s.~s", [Suite, TestCase]),
        " skipped",
        io_lib:format(" (~s.~s failed)", [Suite, Function]),
        format_elapsed_time(StartedAt, EndedAt),
        eol()
    ]),
    ok;
report({skipped, Suite, TestCase, Reason, StartedAt, EndedAt}) ->
    report_test_case(
        color(skipped), ?TEST_SKIPPED_GLYPH, Suite, TestCase, " skipped", StartedAt, EndedAt
    ),
    report_reason(Reason),
    ok.

report_test_case(Color, Glyph, Suite, TestCase, Suffix, StartedAt, EndedAt) ->
    io:put_chars(user, [
        "  ",
        Color,
        Glyph,
        " ",
        io_lib:format("~s.~s", [Suite, TestCase]),
        Suffix,
        format_elapsed_time(StartedAt, EndedAt),
        eol()
    ]).

report_reason(Reason) ->
    io:put_chars(user, [
        format_reason(Reason),
        eol()
    ]).

format_reason(_Reason = {Error, Stack}) when is_list(Stack) ->
    [
        "    with ",
        format_error(Error),
        eol(),
        format_stacktrace(Stack)
    ];
format_reason(Reason) ->
    io_lib:format("~p", [Reason]).

format_error(Error) ->
    % TODO: Truncate this? Borrow some code from lager?
    io_lib:format("~p", [Error]).

format_stacktrace([]) ->
    [];
format_stacktrace([{test_server, _, _, _} | _]) ->
    [];
format_stacktrace([MFA | Stack]) ->
    [format_mfa(MFA), format_stacktrace(Stack)].

format_mfa({M, F, A, Props}) when is_integer(A) ->
    File = proplists:get_value(file, Props),
    Line = proplists:get_value(line, Props),
    format_mfa(M, F, A, File, Line).

format_mfa(M, F, A, undefined, undefined) ->
    io_lib:format("      at ~s:~s/~B~n", [M, F, A]);
format_mfa(M, F, A, File, Line) ->
    io_lib:format("      at ~s:~s/~B (~s, line ~B)~n", [M, F, A, File, Line]).

color(Key) -> get_env_color(Key, get_default_color(Key), os:getenv("NO_COLOR")).

get_default_color(passed) -> ?COLOR_DARK_GREEN;
get_default_color(failed) -> ?COLOR_DARK_RED;
get_default_color(missing) -> ?COLOR_DARK_YELLOW;
get_default_color(skipped) -> ?COLOR_DARK_YELLOW;
get_default_color(elapsed) -> ?COLOR_BRIGHT_BLACK;
get_default_color(_) -> ?COLOR_BRIGHT_CYAN.

% See https://no-color.org/; if NO_COLOR is present and not empty, colours should be disabled.
% i.e. if NO_COLOR is absent or empty, colours should be enabled.
get_env_color(Key, Default, NoColor) when NoColor =:= false; NoColor =:= "" ->
    proplists:get_value(Key, application:get_env(ct_report, colors, []), Default);
get_env_color(_Key, _Default, _NoColor) ->
    "".

format_elapsed_time(StartedAt, EndedAt) when is_integer(StartedAt), is_integer(EndedAt) ->
    format_elapsed_time(EndedAt - StartedAt);
format_elapsed_time(_StartedAt, _EndedAt) ->
    [].

format_elapsed_time(Elapsed) ->
    ElapsedMs = erlang:convert_time_unit(Elapsed, native, millisecond),
    [color(elapsed), " (", format_elapsed_time_ms(ElapsedMs), ")"].

format_elapsed_time_ms(ElapsedMs) ->
    % TODO: Human readable timestamps for longer periods.
    io_lib:format("~Bms", [ElapsedMs]).

eol() ->
    ["\e[0m", "\r\n"].
