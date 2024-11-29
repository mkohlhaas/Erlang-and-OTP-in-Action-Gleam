-module(ring).

-export([run/2, proc/1]).

run(M, N) ->
    % Spawn N processes and connect them as a ring.
    T1 = erlang:system_time(),
    Launcher = self(),
    Pids = [spawn(fun() -> proc(Launcher) end) || _ <- lists:seq(1, N)],
    [HPid | TPids] = Pids,
    RPids = TPids ++ [HPid],
    [Pid ! {next, Next} || {Pid, Next} <- lists:zip(Pids, RPids)],

    io:format("~p processes setup in ~p seconds!~nNow sending ~p messages in a ring!~n", [
        N, execTime(T1, erlang:system_time()), M * N
    ]),

    % Time sending messages around the ring M times.
    T2 = erlang:system_time(),
    HPid ! M * N,
    receive
        done ->
            ok
    end,

    io:format("All messages sent in ~p seconds.~n", [execTime(T2, erlang:system_time())]),

    % Kill all the processes.
    T3 = erlang:system_time(),
    [Pid ! stop || Pid <- Pids],

    io:format("All processes shut down in ~p seconds.~n", [execTime(T3, erlang:system_time())]),

    {ok, all_messages_sent}.

proc(Launcher) ->
    receive
        stop ->
            {ok, stop};
        {next, Pid} ->
            put(next, Pid),
            proc(Launcher);
        0 ->
            Launcher ! done,
            proc(Launcher);
        J ->
            get(next) ! J - 1,
            proc(Launcher)
    end.

execTime(T1, T2) -> (T2 - T1) / 1_000_000_000.
