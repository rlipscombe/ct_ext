-module(ct_ext_report).
-export([report/1]).

-define(APPLICATION, ct_ext).

-include("glyphs.hrl").
-include("colors.hrl").

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

report_test_case(Color, Glyph, Suite, {F, TestCase}, Suffix, StartedAt, EndedAt) when
    F =:= init_per_testcase; F =:= end_per_testcase
->
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
format_reason(_Reason = {'EXIT', Exception}) ->
    [
        "    exited with ",
        ct_ext_format:format_exception(Exception)
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
    [elapsed_color(ElapsedMs), " (", format_elapsed_time_ms(ElapsedMs), ")", ct_ext_color:reset()].

format_elapsed_time_ms(ElapsedMs) ->
    % TODO: Human readable timestamps for longer periods.
    io_lib:format("~Bms", [ElapsedMs]).

elapsed_color(ElapsedMs) ->
    Thresholds = application:get_env(
        ?APPLICATION, elapsed_thresholds, default_elapsed_thresholds()
    ),
    ct_ext_color:threshold(ElapsedMs, Thresholds, ?COLOR_BRIGHT_BLACK).

default_elapsed_thresholds() ->
    % Must be in reverse order by elapsed time.
    [
        {5000, ?COLOR_DARK_RED},
        {2000, ?COLOR_DARK_YELLOW},
        {1000, ?COLOR_DARK_WHITE},
        {0, ?COLOR_BRIGHT_BLACK}
    ].

color(Key) ->
    ct_ext_color:color(Key).

eol() ->
    ct_ext_color:eol().
