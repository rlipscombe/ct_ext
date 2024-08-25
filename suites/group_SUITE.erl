-module(group_SUITE).
-export([
    all/0,
    groups/0
]).
-export([
    test1/1,
    test2/1,
    test3/1
]).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, group1}, {group, group2}].

groups() ->
    [
        {group1, [shuffle], [test1, test2, test3]},
        {group2, [parallel, {repeat, 5}], [test1, test2, test3]}
    ].

test1(_Config) ->
    pass.

test2(_Config) ->
    error(failed).

test3(_Config) ->
    Foo = foo,
    ?assertNotEqual(foo, Foo).
