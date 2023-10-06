-module(ct_ext_debug).
-export([
    id/1,
    init/2,

    post_groups/2,
    post_all/3,

    pre_init_per_suite/3,
    post_init_per_suite/4,

    pre_init_per_group/4,
    post_init_per_group/5,

    pre_init_per_testcase/4,
    post_init_per_testcase/5,

    pre_end_per_testcase/4,
    post_end_per_testcase/5,

    pre_end_per_group/4,
    post_end_per_group/5,

    pre_end_per_suite/3,
    post_end_per_suite/4,

    on_tc_fail/4,
    on_tc_skip/4,

    terminate/1
]).

-define(TRACE(Args), trace(?FUNCTION_NAME, Args)).

id(Opts) ->
    ?TRACE([Opts]),
    ?MODULE.

init(Id, Opts) ->
    ?TRACE([Id, Opts]),
    {ok, no_state}.

post_groups(SuiteName, GroupDefs) ->
    ?TRACE([SuiteName, GroupDefs]),
    GroupDefs.

post_all(SuiteName, Return, GroupDefs) ->
    ?TRACE([SuiteName, Return, GroupDefs]),
    Return.

pre_init_per_suite(SuiteName, InitData, State) ->
    ?TRACE([SuiteName, InitData, State]),
    {InitData, State}.

post_init_per_suite(SuiteName, Config, Return, State) ->
    ?TRACE([SuiteName, Config, Return, State]),
    {Return, State}.

pre_init_per_group(SuiteName, GroupName, InitData, State) ->
    ?TRACE([SuiteName, GroupName, InitData, State]),
    {InitData, State}.

post_init_per_group(SuiteName, GroupName, Config, Return, State) ->
    ?TRACE([SuiteName, GroupName, Config, Return, State]),
    {Return, State}.

pre_init_per_testcase(SuiteName, TestName, InitData, State) ->
    ?TRACE([SuiteName, TestName, InitData, State]),
    {InitData, State}.

post_init_per_testcase(SuiteName, TestName, Config, Return, State) ->
    ?TRACE([SuiteName, TestName, Config, Return, State]),
    {Return, State}.

pre_end_per_testcase(SuiteName, TestName, InitData, State) ->
    ?TRACE([SuiteName, TestName, InitData, State]),
    {InitData, State}.

post_end_per_testcase(SuiteName, TestName, Config, Return, State) ->
    ?TRACE([SuiteName, TestName, Config, Return, State]),
    {Return, State}.

pre_end_per_group(SuiteName, GroupName, InitData, State) ->
    ?TRACE([SuiteName, GroupName, InitData, State]),
    {InitData, State}.

post_end_per_group(SuiteName, GroupName, Config, Return, State) ->
    ?TRACE([SuiteName, GroupName, Config, Return, State]),
    {Return, State}.

pre_end_per_suite(SuiteName, InitData, State) ->
    ?TRACE([SuiteName, InitData, State]),
    {InitData, State}.

post_end_per_suite(SuiteName, Config, Return, State) ->
    ?TRACE([SuiteName, Config, Return, State]),
    {Return, State}.

on_tc_fail(SuiteName, TestName, Reason, State) ->
    ?TRACE([SuiteName, TestName, Reason]),
    State.

on_tc_skip(SuiteName, TestName, Reason, State) ->
    ?TRACE([SuiteName, TestName, Reason]),
    State.

terminate(State) ->
    ?TRACE([State]),
    State.

trace(FunctionName, Args) ->
    io:put_chars(user, io_lib:format("~s(~p)~n", [FunctionName, Args])).
