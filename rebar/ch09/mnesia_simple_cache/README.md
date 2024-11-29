simple_cache
=====

An OTP application

Build
-----

    $ rebar3 compile
    $ rebar3 shell --sname mynode1
    $ [ simple_cache:insert(X,X) || X <- lists:seq(1,10) ].
    $ [ simple_cache:insert(X,X) || X <- lists:seq(11,20) ].
    $ [ simple_cache:lookup(X) || X <- lists:seq(1,20) ].
    $ rebar3 shell --sname mynode2
    $ rebar3 shell --sname mynode3
    $ rebar3 release
    $ rebar3 tar

See also ../../../ch09/mnesia_simple_cache/README.
