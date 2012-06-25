-module(num_tests).

-include_lib("eunit/include/eunit.hrl").

floor_test_() ->
    [ ?_assert( num:floor(10) =:= 10 ),
      ?_assert( num:floor(10.3) =:= 10 ),
      ?_assert( num:floor(10.7) =:= 10 ),
      ?_assert( num:floor(-10.3) =:= -11 ),
      ?_assert( num:floor(-10.7) =:= -11 ) ].

ceil_test_() ->
    [ ?_assert( num:ceil(10.1) =:= 11 ),
      ?_assert( num:ceil(10.7) =:= 11 ),
      ?_assert( num:ceil(-10.1) =:= -10 ),
      ?_assert( num:ceil(-10.7) =:= -10 ) ].

divisible_test_() ->
    [ ?_assert( num:divisible(10, [2,5,6]) ),
      ?_assertNot( num:divisible(11, [2,5,6]) ) ].

divisibleWhile_test_() ->
    Primes = prime:primes(500),
    Pred1 = fun(P) -> P < num:floor( math:sqrt( 1000 )) end,
    Pred2 = fun(P) -> P < num:floor( math:sqrt( 997 )) end,
    [ ?_assert( num:divisibleWhile( 1000, Primes, Pred1 )),
      ?_assertNot( num:divisibleWhile( 997, Primes, Pred2 )) ].

factorial_test_() ->
    [ ?_assertEqual(1, num:factorial(0)),
      ?_assertEqual(1, num:factorial(1)),
      ?_assertEqual(3628800, num:factorial(10)) ].

squares_test_() ->
    [ ?_assertEqual([1,4,9], num:squares(3)),
      ?_assertEqual([1], num:squares(1)) ].

squaresBefore_test_() -> 
    [ ?_assertEqual(9, num:squareBefore(9)),
      ?_assertEqual(9, num:squareBefore(15)),
      ?_assertEqual(16, num:squareBefore(16)) ].

squaresAfter_test_() -> 
    [ ?_assertEqual(16, num:squareAfter(9)),
      ?_assertEqual(16, num:squareAfter(10)),
      ?_assertEqual(25, num:squareAfter(16)) ].

sumOf_test_() ->
    [ ?_assertEqual(55, num:sumOfN(10)),
      ?_assertEqual(14, num:sumOfSquares(3)),
      ?_assertEqual(36, num:sumOfCubes(3)) ].

fibonacci_test_() ->
    Pred = fun(N) -> N =< 20 end,
    [ ?_assertEqual([1,2,3,5,8,13], num:fibonacci(6)),
      ?_assertEqual([1,2,3,5,8,13], num:fibonacci(Pred)) ].

gcd_test_() ->
    [ ?_assertEqual( 10, num:gcd(10, 20) ),
      ?_assertEqual( 1, num:gcd(7, 13) ),
      ?_assertEqual( 22, num:gcd(66, 88) ) ].


binaryGCD_test_() ->
    [ ?_assertEqual( num:binaryGCD(20, 24), num:gcd(20,24) ),
      ?_assertEqual( num:binaryGCD(23412341, 2314123431), num:gcd(23412341, 2314123431) ),
      ?_assertEqual( num:binaryGCD(1232433, 9879879867), num:gcd(1232433, 9879879867) ) ].

factors_test_() ->
    [ ?_assertEqual([1,2,3,5,6,10,15,30], num:factors(30)),
      ?_assertEqual([1,2,3,4,5,6,10,12,15,20,25,30,50,60,75,100,150,300], num:factors(300)),
      ?_assertEqual([1,2,3,5,6,9,10,15,18,30,45,90], num:factors(90)),
      ?_assertEqual([1], num:factors(1)) ].

properFactors_test_() ->
    [ ?_assertEqual([1,2,3,5,6,10,15], num:properFactors(30)),
      ?_assertEqual([1,2,3,4,5,6,10,12,15,20,25,30,50,60,75,100,150], num:properFactors(300)),
      ?_assertEqual([1,2,3,5,6,9,10,15,18,30,45], num:properFactors(90)),
      ?_assertEqual([], num:properFactors(1)) ].

sumOfFactors_test_() ->
    [ ?_assertEqual(72, num:sumOfFactors(30)),
      ?_assertEqual(868, num:sumOfFactors(300)),
      ?_assertEqual(234, num:sumOfFactors(90)),
      ?_assertEqual(1, num:sumOfFactors(1)) ].

primeFactors_test_() ->
    [ ?_assertEqual([2,3,5], num:primeFactors(30)),
      ?_assertEqual([2,3,5], num:primeFactors(300)),
      ?_assertEqual([2,3,5], num:primeFactors(90)),
      ?_assertEqual([], num:primeFactors(1)) ].

is_palindrome_test_() ->
    [ ?_assertNot( num:is_palindrome(123) ),
      ?_assert( num:is_palindrome(1) ),
      ?_assert( num:is_palindrome(121) ) ].

lcm_test_() ->
    [ ?_assertEqual( 60, num:lcm([2,3,4,5]) ) ].

sumOfDigits_test_() ->
    [ ?_assertEqual( 15, num:sumOfDigits(12345) ),
      ?_assertEqual( 1, num:sumOfDigits(1) ) ].

prodOfDigits_test_() ->
    [ ?_assertEqual( 120, num:prodOfDigits(12345) ),
      ?_assertEqual( 1, num:prodOfDigits(1) ) ].

is_pythogTriplet_test_() ->
    [ ?_assertNot( num:is_pythogTriplet(2,3,5) ),
      ?_assert( num:is_pythogTriplet(3,4,5) ) ].

triangleNumbers_test_() ->
    Pred = fun(_N, _TNum, Acc) -> length(Acc) < 20 end,
    [ ?_assertEqual( 210, num:triangleNumber( 20 )),
      ?_assertEqual( 210, lists:nth( 20, num:triangleNumbers( 20 )) ),
      ?_assertEqual( num:triangleNumbers(20), num:triangleNumbers( Pred )) ].

collatz_test_() ->
    [ ?_assertEqual( [], num:collatz(1) ),
      ?_assertEqual( [1], num:collatz(2) ),
      ?_assertEqual( [10,5,16,8,4,2,1], num:collatz(3) ),
      ?_assertEqual( [8,4,2,1], num:collatz(16) ) ].

subractSorted_test_() ->
    [ ?_assertEqual([2,4], num:subractSorted([1,2,3,4,5], [1,3,5] ) ),
      ?_assertEqual([1,2,4,5], num:subractSorted([1,1,2,2,3,4,5,5], [1,2,3,5] ) ) ].

pascalTriangle_test_() ->
    Ref = [ [1],
            [1,1],
            [1,2,1],
            [1,3,3,1],
            [1,4,6,4,1],
            [1,5,10,10,5,1],
            [1,6,15,20,15,6,1],
            [1,7,21,35,35,21,7,1],
            [1,8,28,56,70,56,28,8,1],
            [1,9,36,84,126,126,84,36,9,1] ],
    [ ?_assertEqual(Ref, num:pascalTriangle(10)) ].

amicables_test_() ->
    Pred = fun(N, Acc) -> N =< 6368 end,
    Ref = [220,284,1184,1210,2620,2924,5020,5564,6232,6368],
    [ ?_assertEqual( Ref, num:amicables(10) ),
      ?_assertEqual( Ref, num:amicables(Pred) ) ].

perfectNumbers_test_() ->
    Pred1 = fun(N, _Acc) -> N =< 496 end,
    Pred2 = fun(N, _Acc) -> N =< 54 end,
    Ref = [6,28,496],
    [ ?_assertEqual( Ref, num:perfectNumbers(3) ),
      ?_assertEqual( Ref, num:perfectNumbers(Pred1) ),
      ?_assertEqual( lists:sort( num:perfectNumbers(2) ++ num:abundantNumbers(10) ++ 
                        num:deficientNumbers(42) ),
                     lists:seq(1,54) ),
      ?_assertEqual( lists:sort( num:perfectNumbers(Pred2) ++ num:abundantNumbers(Pred2) ++ 
                        num:deficientNumbers(Pred2) ),
                     lists:seq(1,54) )
    ].

permut_and_combine_test_() ->
    [ ?_assertEqual( 3628800, num:nPn(10) ),
      ?_assertEqual( 1, num:nCn(10) ),
      ?_assertEqual( 720, num:nPr(10, 3) ),
      ?_assertEqual( 120, num:nCr(10, 3) ),
      % Permutations
      ?_assertEqual( [], num:permutations(0, [1, 2]) ),
      ?_assertEqual( [[1], [2]], num:permutations(1, [1,2]) ),
      ?_assertEqual( [[1,2], [2,1]], num:permutations(2, [1,2]) ),
      ?_assertEqual( [], num:permutations(3, [1,2]) ),
      ?_assertEqual( [1,2], num:nthPermutation(0, 2, [1,2,3]) ),
      ?_assertEqual( [2,1], num:nthPermutation(2, 2, [1,2,3]) ),
      % Combinations
      ?_assertEqual( [], num:combinations(0, [1,2,3]) ),
      ?_assertEqual( [[1],[2],[3]], num:combinations(1, [1,2,3]) ),
      ?_assertEqual( [[1,2],[1,3],[2,3]], num:combinations(2, [1,2,3]) ),
      ?_assertEqual( [[1,2,3]], num:combinations(3, [1,2,3]) ),
      ?_assertEqual( [1,2], num:nthCombination(0, 2, [1,2,3]) ),
      ?_assertEqual( [2,3], num:nthCombination(2, 2, [1,2,3]) ) ].

