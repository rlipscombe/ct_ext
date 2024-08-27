-module(ct_ext_elapsed).
-export([
    now/0,
    format_elapsed_time/1,
    format_elapsed_time/2
]).

-include("colors.hrl").

-define(APPLICATION, ct_ext).

now() ->
    erlang:monotonic_time().

format_elapsed_time(Elapsed) ->
    Thresholds = get_elapsed_thresholds(),
    format_elapsed_time(Elapsed, Thresholds).

format_elapsed_time(Elapsed, Thresholds) ->
    ElapsedMs = erlang:convert_time_unit(Elapsed, native, millisecond),
    [
        elapsed_color(ElapsedMs, Thresholds),
        " (",
        format_elapsed_time_ms(ElapsedMs),
        ")",
        ct_ext_color:reset()
    ].

format_elapsed_time_ms(ElapsedMs) when ElapsedMs < 1000 ->
    io_lib:format("~Bms", [ElapsedMs]);
format_elapsed_time_ms(ElapsedMs) when ElapsedMs < 60_000 ->
    Seconds = ElapsedMs div 1000,
    Milliseconds = ElapsedMs rem 1000,
    io_lib:format("~B.~3..0Bs", [Seconds, Milliseconds]);
format_elapsed_time_ms(ElapsedMs) ->
    TotalSeconds = ElapsedMs div 1000,
    Minutes = TotalSeconds div 60,
    Seconds = TotalSeconds rem 60,
    io_lib:format("~B.~3..0Bs", [Minutes, Seconds]).

get_elapsed_thresholds() ->
    application:get_env(?APPLICATION, elapsed_thresholds, default_elapsed_thresholds()).

elapsed_color(ElapsedMs, Thresholds) ->
    ct_ext_color:threshold(ElapsedMs, Thresholds, ?COLOR_BRIGHT_BLACK).

default_elapsed_thresholds() ->
    % Must be in reverse order by elapsed time.
    [
        {5000, ?COLOR_DARK_RED},
        {2000, ?COLOR_DARK_YELLOW},
        {1000, ?COLOR_DARK_WHITE},
        {0, ?COLOR_BRIGHT_BLACK}
    ].
