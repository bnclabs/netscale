-module(nhttp_uri).

-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "nhttp/include/nhttp.hrl" ).

-define( RE_URI_AUTHORITY,
         <<"^(([^:@]+)(:([^:@]+))?@)?([^:]+)(:([0-9]+))?">>
         % [ _, username:pass@, username, :pass, pass, host, :port, port ]
       ).
-define( RE_URI,
         <<"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?">>
         % [ _, scheme:, scheme, //authority, authority, path, ?query, query,
         %     #frag, frag ]
       ).
-define( RE_QUERY,
         <<"^([^=;&]+)(=([^=;&]+))">>
         % [ _, key, =value, value ]
       ).

-define( PERCENT, 37 ).  % $\%
-define( FULLSTOP, 46 ). % $\.
-define( IS_HEX(C),
         ((C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) orelse
          (C >= $A andalso C =< $F) )
       ).
-define( QS_SAFE(C),
         ((C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse
          (C >= $0 andalso C =< $9) orelse
          (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse C =:= $_))
       ).

% Excerpts from RFC2616.txt
%
%   http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
%
% The semantics are that the identified resource is located at the server
% listening for TCP connections on that port of that host, and the Request-URI
% for the resource is abs_path (section 5.1.2). The use of IP addresses

% @doc: HTTP URL into #httpuri{} record and return the same.
uri(parse, "*") -> #httpuri{ type='*' };
uri(parse, []) -> #httpuri{ type=abspath, path="/" };
uri(parse, Data) ->
    case re:run(Data, ?RE_URI, [{capture,all,list}]) of
        {match, Parts} ->
            uri_type( uri_parts( host, Parts, #httpuri{} ));
        _ -> 
            error_logger:error_msg("Invalid URI in request ~p ~n", [Data]),
            #httpuri{type=error}
    end.

% Of what type uri is ?
uri_type(#uri{host=none, path=none}=Uri) ->
    error_logger:error_msg("URI is not properly parsed ~p~n", [Uri]);
uri_type(#uri{host=_Host, path=none}=Uri) -> Uri#uri{type=authority};
uri_type(#uri{host=none, path=_Path}=Uri) -> Uri#uri{type=abspath};
uri_type(#uri{host=_Host, path=_Path}=Uri) -> Uri#uri{type=absolute}.

% Interpret the parsed tokens from uri string.
uri_parts(host, [_, _, Scheme, _, Authority | Parts], Uri) ->
    {User, Pass, Host, Port} = uri_parts( authority, Authority ),
    % TODO : Interpret `Host` as punycode.
    Scheme_ = case Scheme of http -> http; https -> https; _ -> none end,
    uri_parts( path, Parts, 
               Uri#httpuri{ scheme=Scheme_, user=User, pass=Pass, host=Host, 
                            port=Port });

uri_parts(path, [], Uri) -> Uri#httpuri{ path="/" } ;
uri_parts(path, [Path | Parts], Uri) -> 
    Path_ = unquote_plus( case Path of [] -> "/"; X -> X end ),
    uri_parts( quer, Parts, Uri#httpuri{ path=Path_ } );

uri_parts(quer, [], Uri) -> Uri;
uri_parts(quer, [_, Query | Parts], Uri) -> 
    Query_ = parse_qs( case Query of [] -> none; X -> X end ),
    uri_parts( frag, Parts, Uri#httpuri{ quer=Query_ } );

uri_parts(frag, [], Uri) -> Uri;
uri_parts(frag, [_, Frag], Uri) -> Uri;
    Frag_ = unquote_plus( case Frag of [] -> none; "#"++X -> X end ),
    Uri#httpuri{ frag=Frag_ }.

uri_parts(authority, []) -> {none, none, none, none};
uri_parts(authority, Authority) ->
    case re:run(Authority, ?RE_URI_AUTHORITY, [{capture,all,list}]) of
        {nomatch, _} ->
            error_logger:error_msg( "Invalid Authority ~p ~n", [Authority] ),
            {none, none, none, none};
        {match, [_, _, [], _, [], Host | Port]} ->
            { none, none, Host, uri_port(Port) };
        {match, [_, _, User, _, [], Host | Port]} ->
            { User, none, Host, uri_port(Port) };
        {match, [_, _, User, _, Pass, Host | Port]} ->
            { User, Pass, Host, uri_port(Port) }
    end.

uri_port([]) -> none;
uri_port([_, Port]) -> Port.


% GET /pub/WWW/TheProject.html HTTP/1.1
% Host: www.w3.org
uri(netloc, Host, #uri{host=Host}=Uri) -> Uri;
uri(netloc, Host, #uri{host=none}=Uri) -> Uri#uri{ host=Host }.
uri(netloc, Host, Uri) -> Uri.


uri(compose, #httpuri{type="*"}=Uri) -> <<"">>;
uri(compose, #httpuri{type=abspath}=Uri) -> abspath(Uri).
uri(compose, #httpuri{type=authority}=Uri) -> authority(Uri).
uri(compose, #httpuri{type=absolute}=Uri) -> authority(Uri) ++ abspath(Uri).

authority(#httpuri{type=authority, scheme=S,user=U,pass=Pa,host=H,port=P}=Uri)->
    % TODO : Encode `Host` as punycode.
    Scheme = case S of none -> "http"; "" -> "http"; S -> S end,
    User = case U of none -> ""; U -> U end,
    Pass = case Pa of none -> ""; Pa -> ":"++Pa++"@" end,
    Host = case H of none -> ""; H -> H end,
    Port = case P of none -> ""; P -> ":"++P end,
    Scheme ++ "://" ++ User ++ Pass ++ Host ++ Port.

abspath(#uri{path=P, quer=Q, frag=F}=Uri) ->
    Path = case P of none -> "/"; "" -> "/"; P -> urlencode(path, P) end,
    Quer = case Q of none -> ""; "" -> ""; Q -> urlencode(quer, Q end,
    Frag = case F of none -> ""; "" -> ""; F -> quote_plus(F) end,
    Path ++ "?" ++ Quer ++ "#" ++ Frag.


uri(compare, Uri1, Uri2) ->
    lists:all([
        compare_scheme( Uri1#uri.scheme, Uri2#uri.scheme ),
        compare_host( Uri1#uri.host, Uri2#uri.host ),
        compare_port( Uri1#uri.port, Uri2#uri.port ),
        compare_path( Uri1#uri.path, Uri2#uri.path ),
        Uri1#uri.quer = Uri2#uri.quer,
        Uri1#uri.frag = Uri2#uri.frag
    ]).


compare_scheme(S1, S2) -> string:to_lower( S1 ) == string:to_lower( S2 ).
compare_host(H1, H2) -> string:to_lower( H1 ) == string:to_lower( H2 ).
compare_port(Port1, Port2) ->
    case {Port1, Port2} of
        {Port, Port} -> true;
        {none, "80"} -> true;
        {"80", none} -> true;
        {_,_} -> false
    end.
compare_path(Path1, Path2) ->
    case {Path1, Path2} of
        {none, "/"} -> true;
        {"/", none} -> true;
        {Path, Path} -> true;
        {_,_} -> false
    end.


% @doc: Similar to Python's urllib.parse.urlencode()
urlencode(path, Str) when is_list(Str) -> 
    urlencode( unicode:character_to_binary( Str, unicode, utf8 ),
urlencode(path, Bin) -> quote_plus( Bin, "/" );

urlencode(quer, Props) -> 
    string:join( lists:foldr(fun quote_qs/2, [], Props), "&").

quote_qs({K,V}, Acc) ->
    Uni = fun(X) when is_list(X) -> unicode:characters_to_binary(X, unicode, utf8);
             (X) -> X
          end,
    [ quote_plus( Uni(K), [] ) ++ "=" ++ quote_plus( Uni(V), [] ) | Acc ].

quote_plus(Bin, Safe) when is_binary(Bin) ->
    quote_plus( binary_to_list(Bin), Safe, [] );
quote_plus(Str, Safe) -> quote_plus( Str, Safe, [] ).

quote_plus([], Safe, Acc) -> lists:reverse( Acc );
quote_plus([$\s | Rest], Safe, Acc) -> quote_plus( Rest, Safe, [$+ | Acc] );
quote_plus([C | Rest], Safe, Acc) ->
    case ?QS_SAFE(C) orelse lists:member(C, Safe) of
        true ->
            quote_plus( Rest, Safe, [C | Acc] );
        false -> 
            <<Hi:4, Lo:4>> = <<C>>,
            quote_plus(Rest, Safe, [hexdigit(Lo),hexdigit(Hi),?PERCENT | Acc]).
    end.

%% @doc Parse a query string or application/x-www-form-urlencoded.
parse_qs(Bin) when is_binary(Bin) -> parse_qs( binary_to_list( Bin ));
parse_qs(String) ->
    Fn = fun([[_,K,_,V] | Ms]) -> { unquote_plus(K), unquote_plus(V) } end,
    case re:run( String, ?RE_QUERY, [global,{capture,all,list}] ) of
        {match, [Ms]} -> lists:map( Fn, Ms )

%% @doc Unquote a URL encoded string.
unquote_plus([], Acc) ->
    Bin = list_to_binary( lists:reverse( Acc )),
    unicode:character_to_list( Bin, utf8, unicode );
unquote_plus([$+ | Rest], Acc) ->
    unquote_plus( Rest, [$\s | Acc] );
unquote_plus([?PERCENT, Hi, Lo | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
    unquote_plus( Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc] );
unquote_plus([C | Rest], Acc) ->
    unquote_plus( Rest, [C | Acc] ).


hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.


