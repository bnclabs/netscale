-module(nhttp_hdr).


% Multiple message-header fields with the same field-name MAY be
% present in a message if and only if the entire field-value for that
% header field is defined as a comma-separated list [i.e., #(values)].

merge_headers([])       -> [];
merge_headers([H | Hs]) -> merge_headers(H, Hs, []).

merge_headers(H, [], Acc) -> lists:reverse([H|Acc]);
merge_headers(H, Rs, Acc) ->
    case merge_header(H, Rs, []) of
        {NewH, []} -> lists:reverse([NewH | Acc]);
        {NewH, NewRs} -> merge_headers( hd(NewRs), tl(NewRs), [NewH | Acc])
    end.

merge_header({F,V}, [], Hs) -> {{F,V}, lists:reverse(Hs)};
merge_header({F,V1}, [{F,V2} | Rs], Hs) ->
    merge_header({F, string:join([V1, V2], ",")}, Rs, Hs);
merge_header({F,V}, [R | Rs], Hs) ->
    merge_header({F,V}, Rs, [R | Hs]).


% @doc: Parse individual request headers into a list of key,value pairs. Where
% the key will be lower-cased field-name atom and value will be stripped
% string.
%
% Returns,  {ok, Hs}
%           {invalid, HdrData_}
%
% HdrData_ is `lws-replace` HdrData.
parse_headers(HdrData) ->
    HdrData_ = replace_lws( HdrData )
    case re:run( HdrData_, ?RE_HEADER, [global,{capture,all,list}] ) of
        {match, [Hs]} ->
            {ok, merge_headers( parse_header( Hs, [] ))};
        _ -> 
            ?EMSG( "Invalid header in request ~p ~n", [HdrData] ),
            {invalid, HdrData_}
    end.

parse_header([], Acc) -> lists:reverse(Acc);
parse_header([{_,_,Field,Value,_} | Hs], Acc) ->
    F = list_to_atom( string:to_lower( Field )),
    parse_header( Hs, [{F, string:strip(Value)} | Acc] ).


%%---- parse_hdrval() function implementation is incomplete. But the following 
%%---- gives a blue-print of how to parse header values.

%%-- General header fields, Warning and Cache-Control header fields are yet to
%%-- be groked.
parse_hdrval(connection, Value) ->
    {connection, parse_hashrule( Value )};

parse_hdrval(date, Value) ->
    {date, nhttp_date:parse_date( Value )};

parse_hdrval(pragma, Value) ->
    {pragma, parse_pragma( parse_hashrule( Value ))};

parse_hdrval(trailer, Value) ->
    {trailer, parse_hashrule( Value )};

parse_hdrval('transfer-encoding', Value) ->
    {'transfer-encoding', parse_hashrule( Value )};

parse_hdrval(upgrade, Value) ->
    {upgrade, parse_hashrule( Value )};

parse_pragma([V|Vs], Acc) -> lists:reverse(Acc);
parse_pragma([V|Vs], Acc) ->
    case re:run(Value, "([^=]+)(=(.+))?", [{capture,all,list}] ) of
        {match, [_,K,_,V]} -> parse_hdrval(pragma, Vs, [{K,V} | Acc]);
        {match, [_,K]} -> parse_hdrval(pragma, Vs, [k | Acc]);
        _ -> parse_hdrval(pragma, Vs, Acc)
    end.

%%-- Request header fields.
parse_hdrval("accept", Value) ->
    {'accept',
     parse_hdrval( accept, parse_hashrule( Value ))};

parse_hdrval("accept-charset", Value) ->
    {'accept-charset', 
     parse_hdrval( 'accept-charset', parse_hashrule( Value ))}.

parse_hdrval("accept-encoding", Value) ->
    {'accept-encoding', 
     parse_hdrval( 'accept-encoding', parse_hashrule( Value ))}.


parse_hdrval(accept, [], Acc ) -> lists:reverse(Acc);
parse_hdrval(accept, [V|Vs], Acc) ->
    case re:run( V, "([^/]+/[^ \t;]+)(.*)?", [{capture,all,list}] ) of
        {match, [_, MType, Param]} ->
            [{MType, parse_params(Param)} | Acc];
        {match, [_, MType]} ->
            [{MType, []} | Acc];
        _ ->
            ?EMSG("Invalid Accept header in request ~p~n", [V])
    end;

parse_hdrval('accept-charset', [], Acc) -> lists:reverse(Acc);
parse_hdrval('accept-charset', [V|Vs], Acc) ->
    case re:run( V, "([^ \t;]+)(.*)?", [{capture,all,list}] ) of
        {match, [Charset, Param]} ->
            [{Charset, parse_params(Param)} | Acc];
        {match, [Charset]} ->
            [{Charset, []} | Acc];
        _ ->
            ?EMSG("Invalid Accept-Charset header in request ~p~n", [V])
    end;

parse_hdrval('accept-encoding', [], Acc) -> lists:reverse(Acc);
parse_hdrval('accept-encoding', [V|Vs], Acc) ->
    case re:run( V, "([^ \t;]+)(.*)?", [{capture,all,list}] ) of
        {match, [Enc, Param]} ->
            [{Enc, parse_params(Param)} | Acc];
        {match, [Enc]} ->
            [{Enc, []} | Acc];
        _ ->
            ?EMSG("Invalid Accept-Encoding header in request ~p~n", [V])
    end;


% For a field `Field` try to match available list of options with the field`s
% value-specification `ValSpec`,
match_priority(Field, ValSpec, Available) ->
    ok.

% @doc: Replace a single (LWS) linear white space with a single SP
%   LWS = [CRLF] 1*( SP | HT )
replace_lws( HdrData ) ->
    re:replace( HdrData, ?RE_LWS, " ", [global, {return, list}] ).



response_header('server', {}) ->
    Version = application:get_key(?APPNAME, vsn),
    {'Server', "NetscaleHTTP/" ++ Version};

response_header('etag', {weak,Func,Args}) ->
    {'ETag', "W/<opaquetag>"};

response_header('etag', {Func,Args}) ->
    {'ETag', "<opaquetag>"}.
