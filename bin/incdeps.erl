#! /usr/bin/env escript

% This program collects include file dependencies for a given .erl source file
% and generate a corresponding .d file. Similar to GCC.

-module(incdeps).

% If Args contain -I flag of the form,  ** -I <include-dir> ** separate the
% include directories and remaining arguments.
include_flags( [], {Args, Dirs} ) ->
    { lists:reverse(Args), lists:reverse(Dirs) };

include_flags(["-I", Dir | As], {Args, Dirs}) ->
    include_flags( As, {Args, [Dir | Dirs]} );

include_flags([Arg | As], {Args, Dirs}) ->
    include_flags( As, {[Arg | Args], Dirs} ).


% Use Erlang forms library to collect include file (.hrl) dependencies and
% return the list of .hrl files.
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
gather_hrl(File, Is) ->
    case lists:suffix(".hrl", File) of
        true -> [File | Is];
        false -> Is
    end.


main(Args) ->
    {[SourceFile, DepOut], IDirs} = include_flags( Args, {[], []} ),
    {ok, Epp} = epp:open( SourceFile, IDirs ),
    IFiles = forms( Epp, [] ),
    Target = filename:join(
                "ebin",
                filename:basename( filename:rootname(SourceFile) ++ ".beam" )),
    {ok, Fd} = file:open(DepOut, [write]),
    Rule = Target ++ " : " ++ "\\\n",
    IncText = [ "        " ++ IFile ++ "\\\n" || IFile <- IFiles ],
    Text = lists:concat( [Rule] ++ IncText ),
    file:write( Fd, Text ).
