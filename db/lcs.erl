-module(lcs).
-export([ lcs/2, main/1 ]).

max( L1, L2 ) when length(L1) > length(L2) -> L1;
max( _, L2 ) -> L2.

%% Reduce an array by passing offset N to the folding function.
reduce( _, A0, [], _ ) ->
    A0;
reduce( F, A0, [E|Es], N ) ->
    reduce(F, F(A0, E, N), Es, N+1).

reduce( F, A, L ) -> reduce( F, A, L, 0 ).


%% Reduce a 2-dimensional array of N-Rows and M-Columns by computing 
%% Longest Common Subsequence (LCS).
rcfold( A0, Rs, Cs, CSize ) ->
    Get = fun (A, I, J   ) -> array:get( I*CSize+J, A ) end,
    Set = fun (A, I, J, V) -> array:set( I*CSize+J, V, A ) end,
    reduce(
      fun
      (AR, R, I) ->
		 reduce(
		   fun
		   (AC, C, J) when R =:= C ->
			      V = [R | Get(AC, I, J)],
			      Set( AC, I+1, J+1, V );
		   (AC, _, J) ->
			      V = max( Get(AC, I+1, J), Get(AC, I, J+1) ),
			      Set( AC, I+1, J+1, V )
		   end,
		   AR,
		   Cs )
      end,
      A0,
      Rs ).


lcs( Rs, Cs ) ->
    {Rlen, Clen} = {length(Rs), length(Cs)},
    ASize = (Rlen+1) * (Clen+1),
    A0 = array:new( ASize, [ {fixed, true}, {default, []} ] ),
    A1 = rcfold( A0, Rs, Cs, Clen+1 ),
    lists:reverse( array:get( Rlen*(Clen+1)+Clen, A1 )).


edits( [], [],     [],     Acc ) ->
    lists:reverse(Acc);
edits( [C|Lcs]=LCS, [X|Xs]=XXs, [Y|Ys]=YYs, Acc ) ->
    if 
        X =/= C -> edits( LCS, Xs,  YYs, [{d, X} | Acc] );
        Y =/= C -> edits( LCS, XXs, Ys,  [{a, Y} | Acc] );
        true    -> edits( Lcs, Xs,  Ys,  [{e, C} | Acc] )
    end;
edits( [], [X|Xs], Ys,     Acc ) ->
    edits( [], Xs, Ys, [{d, X} | Acc]);
edits( [], [],     [Y|Ys], Acc ) ->
    edits( [], [], Ys, [{a, Y} | Acc]).

edits( Xs, Ys ) -> edits( lcs(Xs, Ys), Xs, Ys, [] ).


%% Test code

linesof( <<>>, [<<>> | Ls] ) ->
    lists:reverse(Ls);
linesof( <<>>, Ls ) ->
    lists:reverse(Ls);
linesof( <<B:8,Tail/binary>>, [L|Ls] ) ->
    if 
        B =:= $\n -> linesof( Tail, [ <<>>, <<L/binary,B:8>> | Ls ]);
        true           -> linesof( Tail, [ <<L/binary,B:8>> | Ls ] )
    end.

main([ File1, File2 ]) ->
    {ok, Text1} = file:read_file( File1 ),
    {ok, Text2} = file:read_file( File2 ),
    Lines1 = linesof( Text1, [<<>>] ),
    Lines2 = linesof( Text2, [<<>>] ),
    io:format("~p ~n", [ edits(Lines1, Lines2) ] ).
