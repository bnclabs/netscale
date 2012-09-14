-module(nhttp_req).

-export([ parse_req/2 ]).


% Request       = Request-Line
%                 *(( general-header | request-header | entity-header ) CRLF)
%                 CRLF
%                 [ message-body ]


% @doc: HTTP uses a "<major>.<minor>" numbering scheme to indicate versions
% of the protocol. The protocol versioning policy is intended to allow
parse_version("HTTP/1.1") -> {1, 1};
parse_version(V) ->
    ?EMSG( "Unsupported HTTP Version in request ~p~n", [V] ),
    throw({status, "505", "Only version HTTP/1.1 is supported"}).


% If Data parsing is completed (see below) or if the received data is empty,
% then return the request record as it is.
parse_request(Req, <<>>) -> Req;
parse_request(Req, []) -> Req;

% Request parsing begins here. And it always starts with a request-line
%   Request-Line = Method SP Request-URI SP HTTP-Version CRLF
% - Ignore preceding CRLFs in request-line.
% - We don't expect request-line to be split across multiple TCP packets.
% - Method, Uri and Version informations are parsed here.
parse_request(#request{state=req_start}=Req, Data) ->
    RE = <<"([\r\n \t]*)(",?RE_TOKEN/binary,") ([^ ]*) ([^\r\n]*)\r\n(.*)">>,
    case re:run( Data, RE, [{capture,all,list}] ) of
        {match, [_, _, Method, Uri, Version, Remn]} ->
            Method = list_to_atom(Method),
            Uri = nhttp_uri:uri(parse, Uri),
            Version = parse_version(Version),

            % Done with request-line, start parsing the headers. And before
            % parsing headers check whether header and body is expected for
            % this request-line.
            case check_eof_request( Req ) of
                true ->
                    Req#request{
                        state=req_end, method=Method, uri=Uri,
                        version=Version, msgbody={0,<<>>},
                        leftover=list_to_binary(Remn)
                    };
                false ->
                    parse_request(
                        Req#request{ state=hdr_start, method=Method, uri=Uri,
                                     version=Version
                        },
                        Remn )
            end;
        {nomatch, X} ->
            ?EMSG( "Invalid request, expecting requestline ~p ~n", [Data] ),
            Req
    end;


parse_request(#request{state=hdr_start}=Req, [$\r, $\n | Data]) ->
    Req#request{state=req_end, msgbody={0,<<>>}, leftover=list_to_binary(Data)};

parse_request(#request{state=hdr_start}=Req, <<"\r\n",Data/binary>>) ->
    Req#request{ state=req_end, msgbody={0, <<>>}, leftover=Data };

parse_request(#request{state=hdr_start}=Req, Data) when is_binary(Data) ->
    doheaders(Data, Req);

parse_request(#request{state=hdr_start}=Req, Data) when is_list(Data) ->
    doheaders(list_to_binary(Data), Req);

parse_request(#request{state=hdr_cont, leftover=HdrData}=Req, Data) ->
    doheaders( <<HdrData/binary,Data/binary>>, Req );
        
parse_request(#request{state=body, msgbody=chunked}=Req, Remn) ->
    ?EMSG("Chunked transfer encoding is not yet supported", [] ),
    throw({status, "400", "Chunked transfer encoding is not yet supported"});

parse_request(#request{state=body, msgbody={X,Body}}=Req, Remn) 
        when size(Body) =:= X ->
    Req#request{ state=req_end, leftover=list_to_binary(Remn) };

parse_request(#request{state=body, msgbody={X,Body}}=Req, Remn) 
        when (size(Body) + length(Remn)) =:= X ->
    Req#request{ state=req_end, 
                 msgbody={X, <<Body/binary,list_to_binary(Remn)/binary>>} };

parse_request(#request{state=body, msgbody={X,Body}}=Req, Remn) 
        when (size(Body) + length(Remn)) < X ->
    Req#request{ msgbody={X, <<Body/binary,list_to_binary(Remn)/binary>>} };

parse_request(#request{state=body, msgbody={X,Body}}=Req, Remn) 
        when (size(Body) + length(Remn)) > X ->
    {Rest1, Rest2} = lists:split( X-size(Body), Remn ),
    Req#request{ state=req_end, 
                 msgbody={X, <<Body/binary,list_to_binary(Rest1)/binary>>},
                 leftover=list_to_binary(Rest2) }.


        [HdrData, Remn] ->
            Hs = nhttp_hdr:parse_headers( HdrData ),
            parse_request(
                Req#request{state=body, hdrs=Hs, msgbody=message_length(Hs)},
                Remn
            ),
        [Remn] ->
            Req#request{state=hdr_cont, leftover=list_to_binary(Remn)}
    end;

doheaders( Data, Req ) ->
    case re:split( Data, "\r\n\r\n", [{return,list},{parts,2}] ) of
        [HdrData, Remn] ->
            case nhttp_hdr:parse_headers( HdrData ) of
                {ok, Hs} ->
                    L = message_length(Hs),
                    parse_request( Req#request{state=body, hdrs=Hs, msgbody=L},
                                   Remn);
                {invalid, _} ->
                    Req#request{state=invalid, leftover=list_to_binary(Remn)}
                    throw({status, "400", "Invalid header in request", Req})
            end;
        [Remn] ->
            Req#request{state=hdr_cont,
                        leftover=<<HdrData/binary,list_to_binary(Remn)/binary>>}
    end.

message_length( #request{method='HEAD'} ) ->
    {0, <<>>};
message_length( #request{hdrs=Hs} ) ->
    case proplists:lookup( 'transfer-encoding', Hs ) of
        {'transfer-encoding', chunked} ->
            chunked;
        none ->
            case proplist:lookup('content-length', Hs) of
                {_, Val} -> {string:to_integer(Val), <<>>};
                _ -> {0, <<>>}
            end
    end.

check_eof_request(#request{method='HEAD'}=Req) -> true;
check_eof_request(Req) -> false;

