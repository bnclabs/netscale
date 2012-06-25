#! /usr/bin/env escript

-module(run_eunit).

run_tests([]) ->
    ok;

run_tests([ "-m", ModName | Tests ]) ->
    eunit:test({module, erlang:list_to_atom(ModName)}, [verbose]),
    run_tests( Tests );

run_tests([ "-a", AppName | Tests ]) ->
    eunit:test({application, erlang:list_to_atom(AppName)}, [verbose]),
    run_tests( Tests ).

main(Tests) ->
    debugHere,
    run_tests( Tests ).
