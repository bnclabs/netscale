#! /usr/bin/env escript

%% Use this program to perform various checks and pre-requisites before
%% making a release. And finally generate a boot-script file for the release.

-module(makerel).

%% An erlang application can have any number of supervisors. And each
%% supervisor can have a child-specification using which it manages its child
%% process. The child specifications are to be defined in the corresponding
%% application's .app file.
supervisor_childspec([]) -> ok;
supervisor_childspec([{K,V}|Es]) ->
    % The paramter name defining the childspec must end with "_sup_childspecs"
    case string:str( atom_to_list(K), "sup_childspecs" ) of
        0 -> supervisor_childspec( Es );
        _ ->
            case supervisor:check_childspecs( V ) of
                ok ->
                    ok;
                {error, Error} ->
                    io:format( "Error : Childspec for supervisor ~w ~n", [K] ),
                    io:format( "Error : Childspec error ~w ~n", [Error] )
            end
    end. 

% Check all child-specifications defined for all applications.
app_childspec([]) -> ok;
app_childspec([AppFile | AppFiles]) ->
    {ok, [Appspec]} = file:consult(AppFile),
    {_Appname, _, Spec} = Appspec,
    case proplists:lookup(env, Spec) of
        {env, []} -> ok;
        {env, Env} -> supervisor_childspec(Env)
    end,
    app_childspec(AppFiles).


%% Make a boot-script out of release file.
make_release(RelFile) ->
    case systools:make_script( RelFile ) of
        ok -> ok;
        error -> halt(1)
    end.


main(Args) ->
    case Args of
        ["release", RelFile] ->
            make_release(RelFile);
        ["childspecs" | AppFiles] ->
            app_childspec(AppFiles);
        ["clash"] ->
            code:clash()
    end.
