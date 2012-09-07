-module(nhttp_req).

-export([ parse_req/2 ]).

% generic-message = request-line
%                   *(message-header CRLF)
%                   CRLF
%                   [ message-body ]
parse_req( State, closed ) ->

parse_req( #nhttps{request=Req}=State, Data ) ->
    parse_headers( parse_reqline( Data, Req ))
            
% Request-Line = Method SP Request-URI SP HTTP-Version CRLF
parse_reqline( Data, #request{state=request_line}=Req ) when size(Data) > 0 ->
    RE = <<"([\r\n \t]*)(",?RE_TOKEN/binary,") ([^ ]*) ([^\r\n]*)\r\n(.*)">>,
    case re:run( Data, RE, [{capture,all,list}] ) of
        {match, [_, _, Method, Uri, Version, Rest]} ->
            Req#httpuri{ method=list_to_atom(Method),
                         uri=nhttp_uri:parse_uri(Uri),
                         version=parse_version(Version)
                       },
            {Req, Rest};
        {nomatch, X} ->
            error_logger:error_msg(
                "Invalid request, expecting requestline ~p ~n",
                [Data] ),
            {Req, []}
    end.
                

% @doc: HTTP uses a "<major>.<minor>" numbering scheme to indicate versions
% of the protocol. The protocol versioning policy is intended to allow
parse_version("HTTP/1.1") -> {1, 1};
parse_version(V) ->
    error_logger:error_msg("Unsupported HTTP Version in request ~p~n", [V]),
    {notsupported, X}.


parse_headers({Req, []}) -> Req;
parse_headers({Req, [$\r, $\n | Rest]) -> {Req#request{hdrs=[]}, Rest};
parse_headers({Req, HdrData}) ->
    case replace_lws( HdrData ) of
        [] -> [];
        HdrData_ ->
            case re:run( HdrData_, ?RE_HEADER, [global,{capture,all,list}] ) of
                {match, [Hs]} ->
                    parse_header(Hs, []);
                _ -> 
                    error_logger:error_msg(
                        "Invalid header in request ~p ~n", [HdrData] )
    end.


parse_header([], Acc) -> lists:reverse(Acc);
parse_header([{_,_,Field,_,Value,_} | Hs], Acc) ->
    parse_header( Hs, [{string:to_lower(Field),Value} | Acc] ).


% Multiple message-header fields with the same field-name MAY be
% present in a message if and only if the entire field-value for that
% header field is defined as a comma-separated list [i.e., #(values)].
merge_multiple_headers(Hs) ->
    ok.

% @doc: Parse #rule structure into a list of `elements`.
% #rule
%   A construct "#" is defined, similar to "*", for defining lists of
%   elements. The full form is "<n>#<m>element" indicating at least
%   <n> and at most <m> elements, each separated by one or more commas
%   (",") and OPTIONAL linear white space (LWS). This makes the usual
%   form of lists very easy; a rule such as
parse_hashrule(Data) when is_list(Data) ->
    lists:map( fun string:strip/1, string:tokens(Data, ",")).

% @doc: Replace a single (LWS) linear white space with a single SP
%   LWS = [CRLF] 1*( SP | HT )
replace_lws( HdrData ) ->
    re:replace( HdrData, ?RE_LWS, " ", [global, {return, list}] ).


