%#! /usr/bin/env escript

-module(termbin).

-record(recordd, { a=10, b=20 }).

terms() -> [
    ok, % atom
    10, % small number
    100000000000000000000000000000000000000000000000000000000, % big number
    10.2, % floating point
    true, % boolean
    false, % boolean
    fun() -> ok end, % function
    <<"hello">>, % bitstring
    <<"hello",10:4>>, % binary
    erlang:make_ref(), % reference
    { nstr, "hello world" }, % string
    #recordd{}, % record
]

    tuple       - Native tuple
    list        - Native list
    port        - Port() class object
    pid         - Pid() class object

main([]) ->
    ok.
