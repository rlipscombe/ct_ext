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

-record(state, {
    run_started_at,
    case_started_at,
    % list of completed test cases, with results
    cases = []
}).

-include("colors.hrl").
-include("glyphs.hrl").

-define(APPLICATION, ct_ext).

init(_Id, _Opts) ->
    % Load application environment.
    application:load(?APPLICATION),

    % For some reason, Erlang doesn't output Unicode correctly when -noinput or -noshell are specified.
    % Fix that by setting the option back.
    io:setopts(standard_io, [{encoding, unicode}]),
    State = #state{run_started_at = ct_ext_elapsed:now()},
    {ok, State}.

add_pass(Suite, TestCase, State = #state{case_started_at = StartedAt, cases = Cases}) ->
    EndedAt = ct_ext_elapsed:now(),
    State#state{cases = [{passed, Suite, TestCase, StartedAt, EndedAt} | Cases]}.

add_failure(Suite, TestCase, Reason, State = #state{case_started_at = StartedAt, cases = Cases}) ->
    EndedAt = ct_ext_elapsed:now(),
    State#state{
        cases = [{failed, Suite, TestCase, Reason, StartedAt, EndedAt} | Cases]
    }.

add_skipped(Suite, TestCase, Reason, State = #state{case_started_at = StartedAt, cases = Cases}) ->
    EndedAt = ct_ext_elapsed:now(),
    State#state{cases = [{skipped, Suite, TestCase, Reason, StartedAt, EndedAt} | Cases]}.

pre_init_per_testcase(_Suite, _TestCase, InitData, State) ->
    {InitData, State#state{case_started_at = ct_ext_elapsed:now()}}.

post_init_per_testcase(
    Suite,
    TestCase,
    _Config,
    Return = {skip, {failed, {_, _, Reason}}},
    State
) ->
    % Called when init_per_testcase fails. Note that we'll report a failure against each affected test (i.e. if
    % init_per_testcase matches on more than one testcase name).
    {Return, add_failure(Suite, {init_per_testcase, TestCase}, Reason, State)};
post_init_per_testcase(_Suite, _TestCase, _Config, Return, State) ->
    {Return, State}.

post_end_per_testcase(
    Suite,
    TestCase,
    _Config,
    Return = ok,
    State
) ->
    {Return, add_pass(Suite, TestCase, State)};
post_end_per_testcase(
    Suite,
    TestCase,
    _Config,
    Return = {failed, {Suite, end_per_testcase, {'EXIT', Reason}}},
    State
) ->
    {Return, add_failure(Suite, TestCase, Reason, State)};
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

on_tc_skip(Suite, TestCase, Reason, State) ->
    add_skipped(Suite, TestCase, Reason, State).

%% We use on_tc_fail rather than post_end_per_testcase, because on_tc_fail also gets notified about init_per_suite,
%% etc., failures.
on_tc_fail(Suite, TestCase, Reason, State) ->
    add_failure(Suite, TestCase, Reason, State).

terminate(State) ->
    report_results(State),
    report_counts(State),
    ok.

report_results(_State = #state{cases = Cases}) ->
    lists:foreach(fun ct_ext_report:report/1, lists:reverse(Cases)),
    io:put_chars(user, [ct_ext_color:reset(), "\r\n"]).

report_counts(State = #state{run_started_at = StartedAt}) ->
    EndedAt = ct_ext_elapsed:now(),
    {PassedCount, SkippedCount, FailedCount} = aggregate_counts(State),
    TotalCount = PassedCount + SkippedCount + FailedCount,
    io:put_chars(user, [
        "  ",
        format_count(PassedCount, passed, ?TEST_PASSED_GLYPH, "passed"),
        ", ",
        format_count(SkippedCount, skipped, ?TEST_SKIPPED_GLYPH, "skipped"),
        ", ",
        format_count(FailedCount, failed, ?TEST_FAILED_GLYPH, "failed"),
        " of ",
        io_lib:format("~B ~s", [TotalCount, pluralize(TotalCount, "case", "cases")]),
        format_total_elapsed_time(StartedAt, EndedAt),
        ct_ext_color:eol()
    ]).

aggregate_counts(_State = #state{cases = Cases}) ->
    lists:foldl(fun aggregate_counts/2, {0, 0, 0}, Cases).

aggregate_counts({passed, _, _, _, _}, {Passed, Skipped, Failed}) ->
    {Passed + 1, Skipped, Failed};
aggregate_counts({skipped, _, _, _, _, _}, {Passed, Skipped, Failed}) ->
    {Passed, Skipped + 1, Failed};
aggregate_counts({failed, _, _, _, _, _}, {Passed, Skipped, Failed}) ->
    {Passed, Skipped, Failed + 1}.

format_count(Count = 0, _Key, _Glyph, Class) ->
    [
        ct_ext_color:reset(),
        ?TEST_NONE_GLYPH,
        " ",
        io_lib:format("~B ~s", [Count, Class]),
        ct_ext_color:reset()
    ];
    format_count(Count, Key, Glyph, Class) ->
    [
        ct_ext_color:color(Key),
        Glyph,
        " ",
        io_lib:format("~B ~s", [Count, Class]),
        ct_ext_color:reset()
    ].

pluralize(Count, Singular, _Plural) when Count == 1 -> Singular;
pluralize(_Count, _Singular, Plural) -> Plural.

format_total_elapsed_time(StartedAt, EndedAt) when is_integer(StartedAt), is_integer(EndedAt) ->
    Thresholds = [{0, ?COLOR_BRIGHT_BLACK}],
    ct_ext_elapsed:format_elapsed_time(EndedAt - StartedAt, Thresholds);
format_total_elapsed_time(_StartedAt, _EndedAt) ->
    [].
