#! /usr/bin/env escript

-module(incdeps).

include_flags( [], {Args, Dirs} ) ->
    { lists:reverse(Args), lists:reverse(Dirs) };

include_flags(["-I", Dir | As], {Args, Dirs}) ->
    include_flags( As, {Args, [Dir | Dirs]} );

include_flags([Arg | As], {Args, Dirs}) ->
    include_flags( As, {[Arg | Args], Dirs} ).

gather_hrl(File, Is) ->
    case lists:suffix(".hrl", File) of
        true -> [File | Is];
        false -> Is
    end.

forms( Epp, Is ) ->
    case epp:parse_erl_form(Epp) of
        {ok, {attribute, _, file, {File, _}}} ->
            forms( Epp, gather_hrl(File, Is) );
        {eof, _} ->
            lists:reverse(Is);
        {ok, _Term} ->
            forms( Epp, Is );
        {error, _Error} ->
            erlang:display(_Error),
            exit(1)
    end.

main(Args) ->
    {[Source, Deps], IDirs} = include_flags( Args, {[], []} ),
    {ok, Epp} = epp:open( Source, IDirs ),
    IFiles = forms( Epp, [] ),
    Target = filename:join(
                "ebin",
                filename:basename( filename:rootname(Source) ++ ".beam" )),
    {ok, Fd} = file:open(Deps, [write]),
    Rule = Target ++ " : " ++ "\\\n",
    IncText = [ "        " ++ IFile ++ "\\\n" || IFile <- IFiles ],
    Text = lists:concat( [Rule] ++ IncText ),
    file:write( Fd, Text ).
