#! /usr/bin/env escript

-module(di).

-include_lib("kernel/include/file.hrl").

parse_options([], Options) ->
    {ok, Cwd} = file:get_cwd(),
    Options_ = case proplists:lookup(name, Options) of
                    none -> [{name, Cwd} | Options];
                    _ -> Options
               end,
    Options_;

parse_options(["-uid" | Args], Options) ->
    Owner = hd(Args),
    parse_options( tl(Args), [{uid, Owner} | Options] );

parse_options(["-name" | Args], Options) ->
    BaseDir = hd(Args),
    parse_options( tl(Args), [{name, BaseDir} | Options] );

parse_options([_ | Args], Options) ->
    parse_options(Args, Options).


do( [], _ ) -> ok;

do([{uid, Owner} | Options], Opts) ->
    Fn = fun(F, Acc) ->
            {ok, Info} = file:read_file_info(F),
            erlang:display(Info#file_info.uid),
            if Info#file_info.uid =:= Owner -> [F | Acc]; true -> Acc end
         end,
    Files = nfile:walk( proplists:get_value( name, Opts ), Fn, [] ),
    writelines( Files ),
    do( Options, Opts );

do([_ | Options], Opts)->
    do( Options, Opts ).


writelines( [] ) -> ok;
writelines([ Line | Lines]) ->
    io:format(Line),
    writelines(Lines).

main(Args) ->
    Options = parse_options(Args, []),
    do( Options, Options ).

