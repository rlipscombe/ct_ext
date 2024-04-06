-module(ct_ext_ensure_started).
-export([
    init/2
]).

-record(state, {}).

init(_Id, Opts) ->
    % Load application environment.
    {ok, _} = application:ensure_all_started(Opts),
    State = #state{},
    {ok, State}.
