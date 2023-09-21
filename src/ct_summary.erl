-module(ct_summary).
-export([
    init/2,
    post_end_per_testcase/5,
    terminate/1
]).

-record(state, {
    cases = []
}).

init(_Id, _Opts) ->
    State = #state{},
    {ok, State}.

post_end_per_testcase(Suite, TestCase, _Config, Return = ok, State = #state{cases = Cases}) ->
    {Return, State#state{cases = [{passed, Suite, TestCase} | Cases]}};
post_end_per_testcase(Suite, TestCase, _Config, Return = {error, _}, State = #state{cases = Cases}) ->
    {Return, State#state{cases = [{failed, Suite, TestCase} | Cases]}}.

terminate(_State = #state{cases = Cases}) ->
    lists:foreach(fun report/1, Cases),
    io:put_chars(user, ["\e[0m", "\r\n"]).

report({passed, Suite, TestCase}) ->
    io:put_chars(user, ["  ", color(passed), io_lib:format("~s.~s", [Suite, TestCase]), " passed", eol()]);
report({failed, Suite, TestCase}) ->
    io:put_chars(user, ["  ", color(failed), io_lib:format("~s.~s", [Suite, TestCase]), " failed", eol()]).

color(passed) ->
    "\e[0;32m";
color(failed) ->
    "\e[0;31m".

eol() ->
    ["\e[0m", "\r\n"].
