-module(num).
-export([
    floor/1, ceil/1, divisible/2, divisibleWhile/3, tau/0,
    factorial/1, squares/1, squareBefore/1, squareAfter/1, sumOfN/1,
    sumOfSquares/1, sumOfCubes/1, fibonacci/1, fibonacciFn/2, gcd/2, binaryGCD/2,
    factors/1, properFactors/1, sumOfFactors/1, 
    primeFactors/1, is_palindrome/1, lcm/1, sumOfDigits/1,
    prodOfDigits/1, is_pythogTriplet/3, triangleNumber/1, 
    triangleNumbers/1, collatz/1, subractSorted/2, pascalTriangle/1,
    gridPath/1, is_amicable/1, amicables/1, amicablesWhile/3, is_perfect/1,
    perfectNumbers/1, deficientNumbers/1, abundantNumbers/1, sumOfAbundants/1,
    nPn/1, nCn/1, nPr/2, nCr/2, sumOfnCn/1, permutations/2,
    nthPermutation/3, combinations/2, nthCombination/3  ]).

-include_lib("nlib/include/common.hrl").

-include_lib("eunit/include/eunit.hrl").

% floor the given floating point number `FNum`.
floor(FNum) when FNum < 0 ->
    T = trunc(FNum),
    case FNum - T of
        0 -> T;
        _ -> T - 1
    end;

floor(FNum) -> 
    trunc(FNum).


% ceil the given floating point number `FNum`.
ceil(FNum) when FNum < 0 ->
    trunc(FNum);

ceil(FNum) ->
    T = trunc(FNum),
    case FNum - T of
        0 -> T;
        _ -> T + 1
    end.


% Division check for n,
%   Given a list of numbers check whether anyof them properly divides `n`.
divisible( _, [] ) -> false;
divisible( N, [X | _ ] ) when (N rem X) == 0 -> true;
divisible( N, [_ | Xs] )  -> divisible( N, Xs ).

divisibleWhile( _, [], _ ) -> false;

divisibleWhile( N, [X | Xs], Pred ) ->
    case Pred(X) of
        true when N rem X == 0 -> true;
        true -> divisibleWhile( N, Xs, Pred );
        false -> false
    end.

% Golden Ratio constant
tau() -> ( 1 + math:sqrt(5) ) / 2.


% Factorial of `n`
factorial(0) -> 1;
factorial(N) -> factorial( N, 1 ).

factorial(0, Acc) -> Acc;
factorial(N, Acc) -> factorial( N-1, Acc * N ).


% Sequential list of square numbers
squares(N) when N >= 1 -> squares( N, [] ).

squares(1, Acc) -> [1 | Acc];
squares(N, Acc) -> squares( N-1, [N*N | Acc] ).


% A square number on or before `n`
squareBefore(N) ->
    M = floor( math:sqrt( N )),
    M * M.


% A square number after `n`
squareAfter(N) ->
    M = ceil( math:sqrt( N )),
    M * M.


% Sum of N natural numbers
sumOfN(N) -> N * (N+1) div 2.


% Sum of N square numbers
sumOfSquares(N) -> N * (N+1) * ((2*N) + 1) div 6.


% Sum of N cube numbers
sumOfCubes(N) -> 
    M = N+1,
    (N*N) * (M*M) div 4.


% Fibonacci
%
% F-8 F-7 F-6 F-5 F-4 F-3 F-2 F-1 F0  F1  F2  F3  F4  F5  F6  F7  F8
% -21 13  -8  5   -3  2   -1  1   0   1   1   2   3   5   8   13  21
%
% TODO : Right now supports only positive fibonacci series.

%% Generate `Count` number of elements in fibonacci sequence.
fibonacci( Count ) when is_integer(Count) ->
    fibonacci(0, 1, Count, []);
%% Generate a fibonacci sequence until the predicate `Pred` is true, where
%% function `Pred` is called for every number in fibonacci sequence.
fibonacci( Pred ) when is_function(Pred) ->
    fibonacciPred(0, 1, Pred, []).
% Generate infinite fibonacci
fibonacciFn(X,Y) -> fun() -> [ X | fibonacciFn(Y, X+Y) ] end.

fibonacci(_, _, 0,     Acc) ->
    lists:reverse(Acc);
fibonacci(N2, N1, Count, Acc) ->
    M = N2+N1,      % Next fibonacci number
    fibonacci(N1, M, Count-1, [M | Acc]).

fibonacciPred(N2, N1, Pred, Acc) ->
    M = N1 + N2,
    case Pred(M) of
        true -> fibonacciPred( N1, M, Pred, [M | Acc] );
        false -> lists:reverse( Acc )
    end.


% GCD
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).


% binaryGCD
%
% The algorithm reduces the problem of finding the GCD by repeatedly applying
% these identities:
%
% 1. gcd(0, v) = v, because everything divides zero, and v is the largest
%    number that divides v. Similarly,
%    gcd(u, 0) = u. gcd(0, 0) is not typically defined, but it is convenient 
%    to set gcd(0, 0) = 0.
% 2. If u and v are both even, then gcd(u, v) = 2·gcd(u/2, v/2), because 2 is
%    a common divisor.
% 3. If u is even and v is odd, then gcd(u, v) = gcd(u/2, v), because 2 is 
%    not a common divisor. Similarly, 
%    if u is odd and v is even, then gcd(u, v) = gcd(u, v/2).
% 4. If u and v are both odd, and u >= v, then gcd(u, v) = gcd((u - v)/2, v).
%    If both are odd and u < v, then gcd(u, v) = gcd((v - u)/2, u). These
%    are combinations of one step of the simple Euclidean algorithm, which
%    uses subtraction at each step, and an application of step 3 above. The
%    division by 2 results in an integer because the difference of two odd
%    numbers is even.
% 5. Repeat steps 2–4 until u = v, or (one more step) until u = 0. In either
%    case, the GCD is 2kv, where k is the number of common factors of 2 found
%    in step 2.
binaryGCD(0, V) -> V;
binaryGCD(U, 0) -> U;
binaryGCD(U, V) when ((U bor V) band 1) == 0 ->
    binaryGCD( U bsr 1, V bsr 1 ) bsl 1;

binaryGCD(U, V) when (U band 1) == 0 ->
    binaryGCD( U bsr 1,   V );

binaryGCD(U, V) when (V band 1) == 0 ->
    binaryGCD( U, V bsr 1 );

binaryGCD(U, V) when U >= V ->
    binaryGCD( U-V bsr 1, V );

binaryGCD(U, V) ->
    binaryGCD( V-U bsr 1, U ).


% Return a list of natural factors for the number
factors(0) -> [];
factors(1) -> [1];
factors(2) -> [2,1];
factors(N) ->
    nutil:prune( factors( N, lists:seq(2, floor( math:sqrt( N ))), [1, N] )).

factors(_, [], Acc) -> lists:sort( lists:reverse( Acc ));
factors(N, [X| Xs], Acc) when N rem X == 0 -> factors(N, Xs, [X, N div X | Acc]);
factors(N, [_| Xs], Acc) -> factors( N, Xs, Acc ).


% Return a list of proper divisors for a number. Proper divisors are numbers
% less than `N` which divide eveny into 
properFactors(1) -> [];
properFactors(2) -> [1];
properFactors(N) ->
    nutil:prune( factors( N, lists:seq(2, floor( math:sqrt( N ))), [1] )).


% Sum of factors
sumOfFactors(N) -> lists:sum( factors(N) ).


% Return a list of prime factors for the number
primeFactors(N) ->
    SRootN = floor( math:sqrt( N )),
    Fn = fun(X, _Acc) -> X =< SRootN end,
    primeFactors( factors(N), prime:primes(Fn), [] ).


primeFactors( [], _, Acc ) ->
    lists:reverse( Acc );

primeFactors( [1 | Fs], Primes, Acc ) ->
    primeFactors( Fs, Primes, Acc );

primeFactors( [F | Fs], Primes, Acc ) ->
    SRootN = floor( math:sqrt( F )),
    Pred = fun (X) -> X =< SRootN end,
    case divisibleWhile( F, Primes, Pred ) of
        true -> primeFactors( Fs, Primes, Acc );
        false -> primeFactors( Fs, Primes, [F | Acc] )
    end.


% Check whether the given number is palindrome in decimal notation.
is_palindrome(N) -> 
    Str = ?ITOA(N),
    Str == lists:reverse( Str ).


% Least common multiple for the given list of numbers.
lcm( [] ) -> 1;
lcm( [A] ) -> A;
lcm( [A, B] ) -> (A*B) div binaryGCD(A, B);
lcm( [A, B | Xs] ) -> lcm([ (A*B) div binaryGCD(A, B) | Xs ]).


% Sum of digits
sumOfDigits( Digits ) when is_list(Digits) ->
    lists:foldl( fun(D, Acc) -> Acc + (D-48) end, 0, Digits );

sumOfDigits( N ) when is_integer(N) ->
    sumOfDigits( ?ITOA(N) ).

% Product of digits
prodOfDigits( Digits ) when is_list(Digits) ->
    lists:foldl( fun(D, Acc) -> Acc * (D-48) end, 1, Digits );

prodOfDigits( N ) when is_integer(N) ->
    prodOfDigits( ?ITOA(N) ).


% Pythogorean triplet
is_pythogTriplet( A, B, C ) -> (A*A + B*B) == (C*C).


% Nth Triangle number.
triangleNumber( N ) -> sumOfN( N ).


% First N Triangle number, or sequence of triangle numbers while predicate
% `Pred` is true.
triangleNumbers( N ) when is_integer(N) -> triangleNumbers( N, [] );
triangleNumbers( Pred ) when is_function(Pred) -> triangleNumbers(1, Pred, []).

triangleNumbers( 0, Acc ) -> Acc;
triangleNumbers( N, Acc ) -> triangleNumbers( N-1, [ sumOfN(N) | Acc ]).

triangleNumbers( N, Pred, Acc ) -> 
    TNum = sumOfN(N),
    factors( TNum ),
    case Pred(N, TNum, Acc) of
        true -> triangleNumbers( N+1, Pred, [TNum | Acc]);
        false -> lists:reverse( Acc )
    end.


% Collatz sequence
collatz(N) -> collatz(N, []).

collatz(1, Acc) -> lists:reverse( Acc );
collatz(N, Acc) when N band 1 == 0 -> collatz( N div 2, [N div 2 | Acc] );
collatz(N, Acc) -> collatz( 3*N + 1, [3*N + 1 | Acc] ).


% Subract sorted
subractSorted( Xs, Ys ) -> subractSorted( Xs, Ys, lists:last(Xs) ).

subractSorted(Xss,      [],          _)             -> Xss;
subractSorted(Xss,      [Y | _],     Last) when Y>Last -> Xss;
subractSorted([X | Xs], [X | Ys],    Last)          -> subractSorted(Xs, Ys, Last);
subractSorted([X | Xs], [Y | _]=Yss, Last) when X<Y -> [X | subractSorted(Xs, Yss, Last)];
subractSorted(Xss,      [_ | Ys],    Last)          -> subractSorted(Xss, Ys, Last).


% Pascal's triangle
pascalTriangle(N) -> pascalTriangle(N-1, [1], [[1]]).

pascalTriangle(0, _, Acc) ->
    lists:reverse(Acc);
pascalTriangle(N, Last, Acc) ->
    Next = pascalTriangle_fold( Last, [1] ),
    pascalTriangle(N-1, Next, [Next | Acc] ).

pascalTriangle_fold([X], Acc) -> [X | Acc];
pascalTriangle_fold([X | [Y|_]=Ys], Acc) -> pascalTriangle_fold(Ys, [X+Y | Acc]).


% Total number of possible shortest paths in a NxN grid. Also called
%   `city grid` (no-backtracking)
gridPath(N) -> lists:max( lists:last( pascalTriangle(2*N+1) )).


% Amicable numbers
amicables(Count) when is_integer(Count) -> amicables( 1, Count, [] );
amicables(Pred) when is_function(Pred) -> amicablesWhile( 1, Pred, [] ).

is_amicable(N) ->
    M = sumOfFactors(N)-N,
    case sumOfFactors(M)-M of
        _ when M == N -> false;
        N -> true;
        _ -> false
    end.

amicables( _, 0, Acc ) -> lists:reverse(Acc);
amicables( N, Count, Acc ) ->
    case is_amicable(N) of
        true -> amicables( N+1, Count-1, [N | Acc] );
        false -> amicables( N+1, Count, Acc )
    end.

amicablesWhile( N, Pred, Acc ) when is_function(Pred) ->
    case Pred(N, Acc) of
        true -> case is_amicable(N) of
                    true -> amicablesWhile( N+1, Pred, [N | Acc] );
                    false -> amicablesWhile( N+1, Pred, Acc )
                end;
        false -> lists:reverse(Acc)
    end.


% Perfect numbers
perfectNumbers(Count) when is_integer(Count) -> perfectNumbers(1, Count, []);
perfectNumbers(Pred) when is_function(Pred) -> perfectNumbersWhile(1, Pred, []).

is_perfect(N) -> 
    case lists:sum( properFactors( N )) of
        M when M == N -> 0;
        M when M > N -> 1;
        M when M < N -> -1
    end.

perfectNumbers(_, 0, Acc) -> lists:reverse(Acc);
perfectNumbers(N, Count, Acc) -> 
    case is_perfect(N) of
        0 -> perfectNumbers(N+1, Count-1, [N | Acc]);
        _ -> perfectNumbers(N+1, Count, Acc)
    end.

perfectNumbersWhile(N, Pred, Acc) -> 
    case Pred(N, Acc) of
        true ->
            case is_perfect(N) of
                0 -> perfectNumbersWhile(N+1, Pred, [N | Acc]);
                _ -> perfectNumbersWhile(N+1, Pred, Acc)
            end;
        false ->
            lists:reverse( Acc )
    end.


% Deficient numbers
deficientNumbers(_, 0, Acc) -> lists:reverse(Acc);
deficientNumbers(N, Count, Acc) -> 
    case is_perfect(N) of
        -1 -> deficientNumbers(N+1, Count-1, [N | Acc]);
        _ -> deficientNumbers(N+1, Count, Acc)
    end.

deficientNumbersWhile(N, Pred, Acc) -> 
    case Pred(N, Acc) of
        true ->
            case is_perfect(N) of
                -1 -> deficientNumbersWhile(N+1, Pred, [N | Acc]);
                _ -> deficientNumbersWhile(N+1, Pred, Acc)
            end;
        false ->
            lists:reverse( Acc )
    end.

deficientNumbers(Count) when is_integer(Count) ->
    deficientNumbers(1, Count, []);

deficientNumbers(Pred) when is_function(Pred) ->
    deficientNumbersWhile(1, Pred, []).


% abundant numbers
abundantNumbers(_, 0, Acc) -> lists:reverse(Acc);
abundantNumbers(N, Count, Acc) -> 
    case is_perfect(N) of
        1 -> abundantNumbers(N+1, Count-1, [N | Acc]);
        _ -> abundantNumbers(N+1, Count, Acc)
    end.

abundantNumbersWhile(N, Pred, Acc) -> 
    case Pred(N, Acc) of
        true ->
            case is_perfect(N) of
                1 -> abundantNumbersWhile(N+1, Pred, [N | Acc]);
                _ -> abundantNumbersWhile(N+1, Pred, Acc)
            end;
        false ->
            lists:reverse( Acc )
    end.

abundantNumbers(Count) when is_integer(Count) ->
    abundantNumbers(1, Count, []);

abundantNumbers(Pred) when is_function(Pred) ->
    abundantNumbersWhile(1, Pred, []).


% List of numbers that can be represented as sum of two abundant numbers.
sumOfAbundants( N ) -> sumOfAbundants( lists:seq(1, N), [], [] ).

sumOfAbundants([], _, Acc) -> lists:reverse(Acc);
sumOfAbundants([N | _]=Nss, As, Acc) ->
    case is_perfect(N) of
        1 -> Ass = [N|As], sumOfAbundants(Nss, Ass, Ass, Acc);
        _ -> sumOfAbundants(Nss, As, As, Acc)
    end.

sumOfAbundants([_ | Ns], [], Ass, Acc) -> sumOfAbundants( Ns, Ass, Acc );
sumOfAbundants([N | Ns]=Nss, [A | As], Ass, Acc) when A =< (N bsr 1) ->
    case is_perfect(N-A) of
        1 -> sumOfAbundants( Ns, Ass, [N | Acc] );
        _ -> sumOfAbundants( Nss, As, Ass, Acc )
    end;
sumOfAbundants(Nss, [_ | As], Ass, Acc) ->
    sumOfAbundants( Nss, As, Ass, Acc ).



% Permutation and combination
nPn(N) -> factorial( N ).
nCn(_) -> 1.
nPr(N, R) -> lists:foldl( fun(X, Acc) -> Acc*X end, 1, lists:seq(N, N-R+1, -1) ).
nCr(N, R) -> nPr(N, R) div factorial(R).


sumOfnCn(N) -> trunc( math:pow(2, N) - 1 ).


%  List of permutations `r` for list of symbols `ls`
permutations(1, Ls) -> lists:map( fun(L) -> [L | []] end, Ls );
permutations(R, Ls) ->
    Permute = fun(L) -> lists:map( 
                            fun(Ls_) -> [L|Ls_] end, 
                            permutations( R-1, lists:delete(L, Ls) )
                        ) end,
    lists:concat( lists:map( Permute, Ls )).


% Nth lexicographic permutation for given list of sorted symbols `ls`
% N starting from 0
nthPermutation(Nth, 1, Ls) -> [ lists:nth(Nth+1, Ls) ];
nthPermutation(Nth, R, Ls) ->
    X = nPr( length(Ls)-1, R-1 ),
    D = Nth div X,
    M = Nth rem X,
    [ lists:nth(D+1, Ls) |
        nthPermutation( M, R-1, lists:delete( lists:nth(D+1, Ls), Ls )) ].
        

% List of combinations `r` for list of symbols `ls`
combinations(1, Ls) -> lists:map( fun(L) -> [L | []] end, Ls );
combinations(R, [_ | Ls]=Lss) ->
    Combine = fun(L, Tss) ->
                lists:map(fun(Ts) -> [L | Ts] end, combinations(R-1, Tss)) end,
    lists:concat( lists:zipwith( 
            Combine, nutil:init(Lss), nutil:init( nutil:tails( Ls )) )).


% Nth lexicographic permutation for given list of sorted symbols `ls`
% N starting from 0
nthCombination( Nth, 1, Ls) -> [ lists:nth(Nth+1, Ls) ];
nthCombination( Nth, R, [L | Ls]) ->
    X = nCr( length(Ls), R-1 ),
    if Nth < X -> [ L | nthCombination(Nth, R-1, Ls) ];
       true -> nthCombination(Nth-X, R, Ls)
    end.

