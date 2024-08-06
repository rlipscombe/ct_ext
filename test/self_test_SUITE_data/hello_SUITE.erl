-module(hello_SUITE).
-export([all/0, hello/1]).

all() -> [hello].

hello(_Config) ->
    ct:pal("Hello World!"),
    ok.
