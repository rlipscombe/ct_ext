-module(ct_ext_summary).
-export([
    init/2,
    pre_init_per_testcase/4,
    post_init_per_testcase/5,
    post_end_per_testcase/5,
    on_tc_skip/4,
    on_tc_fail/4,
    terminate/1
]).

-include("glyphs.hrl").

-record(state, {
    % erlang:monotonic_time, native units.
    case_started_at,
    % list of completed test cases, with results
    cases = []
}).

-define(APPLICATION, ct_ext).

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

post_init_per_testcase(
    Suite,
    _TestCase,
    _Config,
    Return = {skip, {failed, {_, _, Reason}}},
    State = #state{case_started_at = StartedAt, cases = Cases}
) ->
    % Called when init_per_testcase fails. Note that we'll report a failure against each affected test (i.e. if
    % init_per_testcase matches on more than one testcase name).
    EndedAt = erlang:monotonic_time(),
    {Return, State#state{
        cases = [{failed, Suite, init_per_testcase, Reason, StartedAt, EndedAt} | Cases]
    }};
post_init_per_testcase(_Suite, _TestCase, _Config, Return, State) ->
    {Return, State}.

post_end_per_testcase(
    Suite,
    TestCase,
    _Config,
    Return = ok,
    State = #state{case_started_at = StartedAt, cases = Cases}
) ->
    EndedAt = erlang:monotonic_time(),
    {Return, State#state{cases = [{passed, Suite, TestCase, StartedAt, EndedAt} | Cases]}};
post_end_per_testcase(Suite, TestCase, _Config, Return = {failed, {Suite, end_per_testcase, {'EXIT', Reason}}}, State = #state{case_started_at = StartedAt, cases = Cases}) ->
    EndedAt = erlang:monotonic_time(),
    {Return, State#state{cases = [{failed, Suite, {end_per_testcase, TestCase}, Reason, StartedAt, EndedAt} | Cases]}};
post_end_per_testcase(
    _Suite,
    _TestCase,
    _Config,
    Return = {'EXIT', _},
    State
) ->
    % No need; on_tc_skip or on_tc_fail will be called, and since we need on_tc_fail for init_per_suite failures, we'd
    % be better off doing it there.
    {Return, State};
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
    lists:foreach(fun report/1, lists:reverse(Cases)),
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

report_test_case(Color, Glyph, Suite, {F = end_per_testcase, TestCase}, Suffix, StartedAt, EndedAt) ->
    io:put_chars(user, [
        "  ",
        Color,
        Glyph,
        " ",
        io_lib:format("~s.~s (for testcase ~s)", [Suite, F, TestCase]),
        Suffix,
        format_elapsed_time(StartedAt, EndedAt),
        eol()
    ]);
report_test_case(Color, Glyph, Suite, {TestCase, Group}, Suffix, StartedAt, EndedAt) ->
    io:put_chars(user, [
        "  ",
        Color,
        Glyph,
        " ",
        io_lib:format("~s.~s (in group ~s)", [Suite, TestCase, Group]),
        Suffix,
        format_elapsed_time(StartedAt, EndedAt),
        eol()
    ]);
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

format_reason(_Reason = {tc_user_skip, Reason}) ->
    [
        "    with ",
        io_lib:format("~p", [Reason]),
        eol()
    ];
format_reason(_Reason = {Error, Stack}) when is_list(Stack) ->
    [
        "    with ",
        ct_ext_format:format_error(Error),
        eol(),
        ct_ext_format:format_stacktrace(Error, Stack)
    ];
format_reason(Reason) ->
    io_lib:format("    with ~p", [Reason]).

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

color(Key) ->
    ct_ext_color:color(Key).

eol() ->
    ct_ext_color:eol().
