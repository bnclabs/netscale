-module(lcs).
-export([ lcs/2 ]).

nfold( _, A0, [],     _ ) -> A0;
nfold( F, A0, [E|Es], N ) -> nfold(F, F(A0, E, N), Es, N+1).

nfold( F, A, L ) -> nfold(F, A, L, 0).


lcsfold( A0, Rs, Cs, CSize ) ->
    Get = fun (A, I, J   ) -> array:get( I*CSize+J, A ) end,
    Set = fun (A, I, J, V) -> array:set( I*CSize+J, V, A ) end,
    nfold(
      fun
      (A0, R, I) ->
		 nfold(
		   fun
		   (A0, C, J) when R =:= C ->
			      L = [R | Get(I, J, A0)],
			      Set( I+1, J+1, L, A0 );
		   (A0, _, J) ->
			      L = max( Get(I+1, J, A0), Get(I, J+1, A0) ),
			      Set(I+1, J+1, L, A0)
		   end,
		   A0,
		   Cs )
      end,
      A0,
      Rs ).


lcs( Rs, Cs ) ->
    {Rlen, Clen} = {length(Rs), length(Cs)},
    ASize = (Rlen+1) * (Clen+1),
    A0 = array:new( ASize, [ {fixed, true}, {default, []} ] ),
    A1 = lcsfold( A0, Rs, Cs, Clen+1 ),
    lists:reverse( array:get( Rlen*(Clen+1)+Clen, A1 )).


edits( [C|Lcs]=LCS, [X|Xs]=XXs, [Y|Ys]=YYs, Acc ) >
    if 
        X =/= C -> edits( LCS, Xs,  YYs, [{d, X} | Acc] );
        Y =/= C -> edits( LCS, XXs, Ys,  [{a, Y} | Acc] );
        true    -> edits( Lcs, Xs,  Ys,  [{e, C} | Acc] )
    end;
edits( [], [X|Xs], Ys,     Acc ) -> edits( [], Xs, Ys, [{d, X} | Acc]);
edits( [], [],     [Y|Ys], Acc ) -> edits( [], [], Ys, [{a, Y} | Acc]);
edits( [], [],     [],     Acc ) -> lists:reverse(Acc).

edits( Xs, Ys ) -> edits( lcs(Xs, Ys), Xs, Ys, [] ).


Rangeof( I, I ) -> io_lib:format( "~w", [I] );
Rangeof( I, J ) -> io_lib:format( "~w,J", [I,J] );


hunk( Xri, Xrj, Yri, Yrj, Cmd, Hunk, Hunks ) ->
    Hdr = io_lib:format( "~w~w~w~", [ Rangeof(Xri,Xrj), Cmd, Rangeof(Yri,Yrj) ]),
    Hunk_ = [ Hdr | lists:reverse( Hunk ) ],
    hunkify( Xi+1, Yj+1, Es, [ Hunk_ | Hunks ] ).


%% end-of-edit
hunkify( Xi, Yj, [], {Xri, Xrj}=Xr, Cmd, {Yri, Yrj}=Yr, Hunk, Hunks ) ->
    hunk( Xri, Xrj, Yri, Yrj, Cmd, Hunk, Hunks );
%% Delete-hunk, continue deleting
hunkify( Xi, Yj, [{d,Ch}|Es], {Xri, Xrj}=Xr, d, Yr, Hunk, Hunks ) ->
    hunkify( Xi+1, Yj, Es, {Xri, Xi}, d, Yr, ["< "++[Ch] | Hunk], Hunks ] );
%% Delete-hunk, end of delete-hunk
hunkify( Xi, Yj, [{e,Ch}|Es], {Xri, Xrj}=Xr, d, {Yri, Yrj}=Yr, Hunk, Hunks ) ->
    hunkify( Xi+1, Yj+1, Es, hunk(Xri, Xrj, Yri, Yrj, d, Hunk, Hunks) );
%% Delete-hunk, change-over
hunkify( Xi, Yj, [{a,Ch}|Es], Xr, d, {Yri, Yrj}=Yr, Hunk, Hunks ) ->
    Hunk_ = [ "> "++[Ch], "----" ] ++ Hunk,
    hunkify( Xi, Yj+1, Es, Xr, a, {Yri,Yj}, Hunk_, Hunks ] );
%% Add-hunk, continue adding
hunkify( Xi, Yj, [{a,Ch}|Es], Xr, a, {Yri, Yrj}=Yr, Hunk, Hunks ) ->
    hunkify( Xi, Yj+1, Es, Xr, a, {Yri,Yj}, ["> "++[Ch] | Hunk], Hunks ] );
%% Add-hunk, end of add-hunk
hunkify( Xi, Yj, [{e,Ch}|Es], {Xri, Xrj}=Xr, a, {Yri, Yrj}=Yr, Hunk, Hunks ) ->
    hunkify( Xi+1, Yj+1, Es, hunk(Xri, Xrj, Yri, Yrj, a, Hunk, Hunks) );
%% Add-hunk, change-over
hunkify( Xi, Yj, [{d,Ch}|Es], {Xri, Xrj}=Xr, a, Yr, Hunk, Hunks ) ->
    Hunk_ = [ "< "++[Ch], "----" ] ++ Hunk,
    hunkify( Xi+1, Yj, Es, {Xri, Xi}, a, Yr, Hunk_, Hunks ] );

%% Make edit hunks
hunkify( Xi, Yj, [{e,_}|Es], Hunks ) ->
    Hunks
hunkify( Xi, Yj, [{e,_}|Es], Hunks ) ->
    hunkify( Xi+1, Yj+1, Es, Hunks );
hunkify( Xi, Yj, [{d,Ch}|Es], Hunks ) ->    % Start a delete-hunk
    hunkify( Xi+1, Yj, Es, {Xi,Xi}, d, {Yj,Yj}, [ "< "++[Ch] ], [] );
hunkify( Xi, Yj, [{a,Ch}|Es], Hunks ) ->    % Start a add-hunk
    hunkify( Xi, Yj+1, Es, {Xi,Xi}, a, {Yj,Yj}, [ "> "++[Ch] ], [] );
