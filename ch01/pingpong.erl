-module(pingpong).

-export([run/0]).

-spec run() -> ok.
run() ->
    Pid = spawn(fun ping/0),
    Pid ! self(),
    receive
        pong -> ok
    end.

-spec ping() -> pong.
ping() ->
    receive
        From -> From ! pong
    end.
