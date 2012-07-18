-module(run_eunit).

-include_lib("eunit/include/eunit.hrl").
-export([ start/1 ]).

options(Args, Options, Tests) ->
    case Args of
        [] -> {lists:reverse(Options), lists:reverse(Tests)};
        ['/v' | Args1 ] -> options( Args1, [verbose | Options], Tests );
        ['/all' | Args1 ] -> options( Args1, [all | Options], Tests );
        [Test | Args1] -> options( Args1, Options, [Test | Tests] )
    end.

test_apps([], _Options) -> ok;
test_apps([ AppName | Apps ], Options) ->
    ?debugFmt("Executing Unit test for app `~s` ...", [AppName]),
    eunit:test({application, AppName}, Options),
    test_apps( Apps, Options ).


test_mods([], _Options) -> ok;
test_mods([ ModName | Mods ], Options) ->
    ?debugFmt("Executing Unit test for module `~s` ...", [ModName]),
    eunit:test({module, ModName}, Options),
    test_mods( Mods, Options ).

run_tests([], _Options) -> ok;
run_tests(['/m', ModName | Tests ], Options) ->
    test_mods([ModName], Options),
    run_tests(Tests, Options);
run_tests(['/a', AppName | Tests ], Options) ->
    test_apps([AppName], Options),
    run_tests(Tests, Options).

start(Args) ->
    {Options, Tests} = options(Args, [], []),
    case lists:member( all, Options ) of
        true ->
            SystemApps = [stdlib, kernel],
            Apps = proplists:get_keys( application:loaded_applications() ),
            test_apps( Apps -- SystemApps, Options);
        false ->
            run_tests( Tests, Options )
    end,
    init:stop().
