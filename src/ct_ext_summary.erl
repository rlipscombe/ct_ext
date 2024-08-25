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

add_pass(Suite, TestCase, State = #state{case_started_at = StartedAt, cases = Cases}) ->
    EndedAt = erlang:monotonic_time(),
    State#state{cases = [{passed, Suite, TestCase, StartedAt, EndedAt} | Cases]}.

add_failure(Suite, TestCase, Reason, State = #state{case_started_at = StartedAt, cases = Cases}) ->
    EndedAt = erlang:monotonic_time(),
    State#state{
        cases = [{failed, Suite, TestCase, Reason, StartedAt, EndedAt} | Cases]
    }.

add_skipped(Suite, TestCase, Reason, State = #state{case_started_at = StartedAt, cases = Cases}) ->
    EndedAt = erlang:monotonic_time(),
    State#state{cases = [{skipped, Suite, TestCase, Reason, StartedAt, EndedAt} | Cases]}.

pre_init_per_testcase(_Suite, _TestCase, InitData, State) ->
    {InitData, State#state{case_started_at = erlang:monotonic_time()}}.

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

terminate(_State = #state{cases = Cases}) ->
    lists:foreach(fun ct_ext_report:report/1, lists:reverse(Cases)),
    io:put_chars(user, [ct_ext_color:reset(), "\r\n"]).
