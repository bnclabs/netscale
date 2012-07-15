#! /usr/bin/env escript
%%! -sname script_peul

-include_lib("ncloud/include/ncloud.hrl").

-mode(compile).

problem( 1 ) ->
    IsDiv35 = fun(X) -> (X rem 3 == 0) orelse (X rem 5 == 0) end,
    lists:sum( lists:filter( IsDiv35, lists:seq(3,999) ));

problem( 2 ) ->
    Pred = fun (X) -> X =< 4000000 end,
    Nlist = num:fibonacci( Pred ),
    lists:sum( lists:filter( fun(X) -> X rem 2 == 0 end, Nlist ));

problem( 3 ) ->
    lists:max( num:primeFactors( 600851475143 ));

problem( 4 ) ->
    Ls = lists:seq(999, 900, -1),
    Fn = fun (X) -> not num:is_palindrome( X ) end,
    SortFn = fun(A, B) -> A > B end,
    hd(lists:dropwhile( Fn, lists:sort( SortFn, [ X*Y || X<-Ls, Y<-Ls ]) ));

problem( 5 ) ->
    num:lcm( lists:seq(1,20) );

problem( 6 ) ->
    X = num:sumOfN(100),
    (X*X) - num:sumOfSquares(100);

problem( 7 ) ->
    lists:last( num:primes(10001) );

problem( 8 ) ->
    {ok, [Digits]} = file:consult("/home/pratap/dev/blob/eulerdata/prob8.txt"),
    lists:max( digits5( Digits, [] ));
            
problem( 9 ) ->
    Xs = [ {A, B, 1000-A-B} || A <- lists:seq(4,1000), B <- lists:seq(A,1000) ],
    Fn = fun ({A, B, C}) -> not num:is_pythogTriplet(A, B, C) end,
    {X,Y,Z} = hd( lists:dropwhile( Fn, Xs )),
    X*Y*Z;

problem( 10 ) ->
    Fn = fun (P, _) -> P < 2000000 end,
    lists:sum( num:primes(Fn) );

problem( 11 ) ->
    {ok, [_Arr]} = file:consult("/home/pratap/dev/blob/eulerdata/prob11.txt"),
    "To be implemented";

problem( 12 ) ->
    Pred = fun(_, TNum, _) -> length( num:factors(TNum) ) =< 500 end,
    num:sumOfN( length( num:triangleNumbers( Pred )) + 1 );

problem( 13 ) ->
    {ok, Numbers} = file:consult("/home/pratap/dev/blob/eulerdata/prob13.txt"),
    {Res, _} = lists:split(10, ?ITOA( num:sumOfDigits( Numbers ))),
    ?ATOI( Res );

problem( 14 ) ->
    longestCollatz( lists:seq(2, 999999), {1, 0} );

problem( 15 ) ->
    num:gridPath( 20 );

problem( 16 ) ->
    Digits = ?ITOA( lists:foldl( fun(_, A) -> A*2 end, 2, lists:seq(1,999))),
    num:sumOfDigits( Digits );

problem( 17 ) ->
    "To be implemented";

problem( 18 ) ->
    {ok, Triangle} = file:consult("/home/pratap/dev/blob/eulerdata/prob18.txt"),
    ReduceFn = fun 
                (Y, A) ->
                    S = lists:zipwith( fun(J,K) -> J+K end, Y, A ),
                    M = tl(S), 
                    N = lists:reverse( tl( lists:reverse( S ))),
                    if 
                        length(S) == 1 -> S;
                        true -> lists:zipwith( fun erlang:max/2, M, N )
                    end
               end,
    lists:foldr( ReduceFn, lists:duplicate(15, 0), Triangle );

problem( 19 ) ->
    Ms = lists:seq(1, 12),
    Ys = lists:seq(1901,2000),
    Fn = fun(X) -> X == 7 end,
    Days = [ calendar:day_of_the_week(Y, M, 1) || M <- Ms, Y <- Ys ],
    length( lists:filter( Fn, Days ));

problem( 20 ) ->
    num:sumOfDigits( ?ITOA( num:factorial( 100 )) );

problem( 21 ) ->
    lists:sum( num:amicables( fun(X, _) -> X < 10000 end ));

problem( 22 ) ->
    {ok, [Names]} = file:consult("/home/pratap/dev/blob/eulerdata/prob22.txt"),
    Index = lists:seq( 1, length(Names) ),
    NormFn = fun(X) -> X - 64 end,
    Fn = fun({I, S}) -> I * lists:sum( lists:map( NormFn, S )) end,
    lists:sum( lists:map( Fn, lists:zip( Index, lists:sort(Names) )));

problem( 23 ) ->
    S0 = lists:sum( num:sumOfAbundants( 28123 ) ),
    S1 = lists:sum( lists:seq(1, 28123) ),
    S1 - S0.

main( [Arg | _Args] ) ->
    case Arg of
        "all" -> ok;
        _N    -> io:format( "~w ~n", [ problem(?ATOI(Arg)) ] )
    end.


digits5( [A, B, C, D, E | Xs], Acc ) ->     % For problem 8
    digits5( [B,C,D,E | Xs], [ num:prodOfDigits([A,B,C,D,E]) | Acc ] );
digits5( _, Acc ) ->
    Acc.

longestCollatz([], Res) ->
    Res;
longestCollatz([X | Xs], {_, Len}=Res) ->
    case num:collatz(X) of
        Ls when length(Ls) > Len -> longestCollatz( Xs, {X, length(Ls)} );
        _Ls -> longestCollatz( Xs, Res )
    end.

