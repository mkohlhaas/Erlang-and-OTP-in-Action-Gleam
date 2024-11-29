% app starts (ets) store and supervisor
-module(sc_app).

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sc_store:init(),
    case sc_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Other -> {error, Other}
    end.

stop(_State) -> ok.
