#! /usr/bin/env escript

-module(di).

setup( Base ) ->
    NSDir = filename:join( Base, ".netscale" ),
    DIDir = filename:join( NSDir, "di" ),
    {ok, Tbl} = 
    check_create_dir( NSDir ),
    check_create_dir( DIDir ).

check_create_dir( Dir ) ->
    case file_lib:is_dir( Dir ) of
        false -> file:make_dir( Dir ),
        true -> ok
    end.

emitfile( File ) ->
    ok.


main([ Base ]) ->
    setup( Base ),
    nfile:walk( Base, fun emitfile/1 ),
    ok.
