-module(die_please1).

-export([go/0]).

go() ->
    % matching will fail and cause an exception (on purpose)
    i_really_want_to_die = right_now.
