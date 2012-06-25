#! /usr/bin/env escript

-module(makerel).

check_childspec([]) -> ok;
check_childspec([AppFile | AppFiles]) ->
    {ok, [Appspec]} = file:consult(AppFile),
    {Appname, _, Spec} = Appspec,
    case proplists:lookup(env, Spec) of
        {env, []} -> ok;
        {env, Env} ->  
            {childspecs, ChildSpecs} = proplists:lookup(childspecs, Env),
            case supervisor:check_childspecs( ChildSpecs ) of
                ok ->
                    ok;
                {error, Error} ->
                    io:format( "Error : Childspec for App ~w ~n", [Appname] ),
                    io:format( "Error : Childspec error ~w ~n", [Error] )
            end
    end,
    check_childspec(AppFiles).

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
            check_childspec(AppFiles);
        ["clash"] ->
            code:clash()
    end.
