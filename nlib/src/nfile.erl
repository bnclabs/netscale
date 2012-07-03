-module(nfile).
-export([ walk/2 ]).

-include_lib("kernel/include/file.hrl").

walk( Path, Callback ) ->
    Fn = fun(E) ->
            Name = filename:join(Path, E),
            Callback(Name),
            case filelib:is_dir(Name) of
                true -> walk( Name, Callback );
                false -> ok
            end
         end,
    case file:list_dir(Path) of
        {ok, Entries} -> lists:map( Fn, Entries );
        {error, eacces} -> ok
    end.
