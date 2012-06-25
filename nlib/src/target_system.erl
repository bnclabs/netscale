-module(target_system).

%% Note: RelFileName below is the *stem* without trailing .rel,
%% .script etc.
%%

%% create(RelFileName)
%%

%create( RelFileName ) -> create( RelFileName, [] ).

%create( RelFileName, SystoolsOpts ) ->
    %RelFile = RelFileName ++ ".rel", 
    %Dir = filename:dirname( RelFileName ),
    %PlainRelFileName = filename:join( Dir, "plain" ),
    %PlainRelFile = PlainRelFileName ++ ".rel",
    %io:format( "Reading file: ~p ...~n", [RelFile] ),
    %{ok, [RelSpec]} = file:consult( RelFile ),
    %io:format( "Creating file: ~p from ~p ...~n", [PlainRelFile, RelFile] ),
    %{release,
    %    {RelName, RelVsn},
    %    {erts, ErtsVsn},
    %    AppVsns} = RelSpec,
    %Fn = fun({kernel, _}) -> true; 
    %        ({stdlib, _}) -> true;
    %        (_) -> false
    %     end,
    %PlainRelSpec = { release,
    %                    {RelName, RelVsn},
    %                    {erts, ErtsVsn},
    %                    lists:filter(Fn, AppVsns)
    %               },
    %

    %{ok, Fd} = file:open( PlainRelFile, [write] ),
    %io:format( Fd, "~p.~n", [PlainRelSpec] ),
    %file:close(Fd),
    %io:format( "Making \"~s.script\" and \"~s.boot\" files ...~n",
    %           [PlainRelFileName, PlainRelFileName] ),
    %make_script(PlainRelFileName, SystoolsOpts),
    %io:format( "Making \"~s.script\" and \"~s.boot\" files ...~n", 
    %           [RelFileName, RelFileName] ),
    %make_script( RelFileName, SystoolsOpts ),

    %{ok, Fd} = file:open(PlainRelFile, [write]),
    %io:format( Fd, "~p.~n", [PlainRelSpec]) ,
    %file:close(Fd),

    %io:format( "Making \"~s.script\" and \"~s.boot\" files ...~n",
    %           [ PlainRelFileName,PlainRelFileName ] ),
    %make_script( PlainRelFileName, SystoolsOpts ),

    %io:format( "Making \"~s.script\" and \"~s.boot\" files ...~n", 
    %           [ RelFileName, RelFileName] ),
    %make_script( RelFileName, SystoolsOpts ),

    %io:format( "Extracting ~p into directory ~p ...~n", [TarFileName,TmpDir] ),
    %extract_tar( TarFileName, TmpDir ),

    %TmpBinDir = filename:join([TmpDir, "bin"]),
    %ErtsBinDir = filename:join([TmpDir, "erts-" ++ ErtsVsn, "bin"]),
    %io:format( "Deleting \"erl\" and \"start\" in directory ~p ...~n", 
    %           [ErtsBinDir] ),
    %file:delete( filename:join( [ErtsBinDir, "erl"] )),
    %file:delete( filename:join( [ErtsBinDir, "start"] )),

    %io:format( "Creating temporary directory ~p ...~n", [TmpBinDir] ),
    %file:make_dir( TmpBinDir ),

    %io:format( "Copying file \"~s.boot\" to ~p ...~n", 
    %           [PlainRelFileName, filename:join([TmpBinDir, "start.boot"])] ),
    %copy_file( PlainRelFileName++".boot",filename:join([TmpBinDir, "start.boot"]) ),

    %io:format( "Copying files \"epmd\", \"run_erl\" and \"to_erl\" from \n" "~p to ~p ...~n",
    %           [ErtsBinDir, TmpBinDir] ),
    %copy_file( filename:join( [ErtsBinDir, "epmd"] ), 
    %           filename:join( [TmpBinDir, "epmd"] ),
    %           [preserve] ),
    %copy_file( filename:join([ErtsBinDir, "run_erl"] ), 
    %           filename:join([TmpBinDir, "run_erl"]),
    %           [preserve] ),
    %copy_file( filename:join( [ErtsBinDir, "to_erl"] ), 
    %           filename:join([TmpBinDir, "to_erl"]),
    %           [preserve] ).
