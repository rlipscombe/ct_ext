-module(ct_ext_color).
-export([
    color/1,
    threshold/3,
    reset/0,
    eol/0
]).

-define(APPLICATION, ct_ext).
-include("colors.hrl").

color(Key) ->
    get_env_color(Key, get_default_color(Key), is_color_enabled()).

is_color_enabled() ->
    is_color_enabled(os:getenv("NO_COLOR")).

is_color_enabled(false) -> true;
is_color_enabled("") -> true;
is_color_enabled(_) -> false.

get_default_color(passed) -> ?COLOR_DARK_GREEN;
get_default_color(failed) -> ?COLOR_DARK_RED;
get_default_color(missing) -> ?COLOR_DARK_YELLOW;
get_default_color(skipped) -> ?COLOR_DARK_YELLOW;
get_default_color(_) -> ?COLOR_BRIGHT_CYAN.

% See https://no-color.org/; if NO_COLOR is present and not empty, colours should be disabled.
% i.e. if NO_COLOR is absent or empty, colours should be enabled.
get_env_color(Key, Default, true) ->
    proplists:get_value(Key, application:get_env(?APPLICATION, colors, []), Default);
get_env_color(_Key, _Default, false) ->
    "".

threshold(Value, Thresholds = [{_, _} | _], Default) ->
    threshold(Value, Thresholds, Default, is_color_enabled()).

threshold(_Value, _Thresholds, _Default, false) ->
    "";
threshold(Value, Thresholds, Default, true) ->
    % Thresholds is expected to be sorted in reverse order.
    case lists:search(fun({Threshold, _}) -> Value >= Threshold end, Thresholds) of
        {value, {_, Color}} ->
            Color;
        _ ->
            Default
    end.

reset() ->
    reset(is_color_enabled()).

reset(true) -> "\e[0m";
reset(false) -> "".

eol() ->
    eol(is_color_enabled()).

eol(true) -> ["\e[0m", "\r\n"];
eol(false) -> ["\r\n"].
