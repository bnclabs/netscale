#! /usr/bin/env escript

-module(run_eunit).

run_tests([]) ->
    ok;

run_tests([ "-m", ModName | Tests ]) ->
    _Options = [ verbose ],
    eunit:test({module, erlang:list_to_atom(ModName)}),
    run_tests( Tests );

run_tests([ "-a", AppName | Tests ]) ->
    _Options = [ verbose ],
    eunit:test({application, erlang:list_to_atom(AppName)}),
    run_tests( Tests ).

main(Tests) ->
    debugHere,
    run_tests( Tests ).
