-module(nutil_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ncloud/include/ncloud.hrl").

fst_test_() ->
    [ ?_assertEqual(1, nutil:fst({1,2})) ].

snd_test_() ->
    [ ?_assertEqual(2, nutil:snd({1,2})) ].

prune_test_() ->
    [ ?_assertEqual([1,2,3,4,5], nutil:prune([1,1,2,2,3,4,4,5])),
      ?_assertEqual([1,2,3,4,5], nutil:prune([1,2,2,3,4,4,5,5])) ].

tails_test_() ->
    Ref = [ [1,2,3,4,5], [2,3,4,5], [3,4,5], [4,5], [5], [] ],
    [ ?_assertEqual( Ref, nutil:tails([1,2,3,4,5 ]) ) ].

init_test_() ->
    [ ?_assertEqual( [1,2,3,4], nutil:init([1,2,3,4,5]) ) ].


