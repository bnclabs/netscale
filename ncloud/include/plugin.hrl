-include_lib( "ncloud/include/ncloud.hrl" ).

-export([ init/1, attr/1, attr/2 ]).

attr(Key) ->
    Attrs = get( attributes ),
    proplists:lookup( Attrs, Key ).


attr(Key, Val) ->
    erase( Key ),
    put( attributes, [{Key, Val} | get(attributes)] ).


init( Args ) ->
    process_flag( trap_exit, true ),
    put( init_args, Args ),
    put( attributes, [] ),
    ?MODULE:initialize( Args ).
