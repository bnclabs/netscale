-module(nutil).

-export([ fst/1, snd/1, prune/1, tails/1, init/1, init_last/1 ]).

fst(Tuple) -> element(1, Tuple).    % First element of tuple `Tuple`
snd(Tuple) -> element(2, Tuple).    % Second element of tuple `Tuple`

% Prune duplicates in sorted list
prune(List) -> prune( List, [] ).

prune([], Acc) -> lists:reverse( Acc );
prune([X | [X | _]=Xss], Acc) -> prune(Xss, Acc);
prune([X | Xs], Acc) -> prune(Xs, [X | Acc]).


% Calculate the tails of a list.
tails(Ls) -> [ lists:nthtail(X, Ls) || X <- lists:seq(0, length(Ls)) ].


% Initial list except the last element.
init(Ls) -> init(Ls, []).

init([], _) -> [];
init([_ | []], Acc) -> lists:reverse(Acc);
init([L | Ls], Acc) -> init( Ls, [L | Acc] ).


% Return a tuple of initial list and last element.
init_last( Ls ) -> init_last( Ls, [] ).

init_last([], _) -> erlang:error("Empty list to init_last");
init_last([L | []], Acc) -> {lists:reverse( Acc ), L};
init_last([L | Ls], Acc) -> init_last( Ls, [L | Acc] ).

