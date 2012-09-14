-module(nhttp_resp).

-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "nhttp/include/nhttp.hrl" ).

% @doc: Only HTTP version 1.1 is supported.
version() -> "HTTP/1.1".

start_response(Sock, Code, Hdrs) ->
    start_response(Sock, Code, reason(Code), Hdrs).

start_response(Sock, "500", Reason, Hdrs) ->
    Resp = start_response(Code, reason(Code), Hdrs),
    Data = packetize( 
             Resp#response{<<"`nhttp` server supports only HTTP/1.1 version">>}
           ),
    gen_tcp:send( Sock, Data ).

start_response(Sock, Code, Reason, Hdrs) ->
    Hdrs_ = [ nhttp_hdr:response_header('Server')
             | Hdrs ],
    Resp = #response{version=version(), stcode=Code, reason=Reason, hdrs=Hdrs_},
    Data = <<packetize(stline, Resp)/binary,packetize(headers,Resp)/binary>>,
    gen_tcp:send(Sock, Data).


send_response(Sock, Hdrs, Body) ->
    gen_tcp:send(Sock, packetize(msgbody)).


packetize(#response{msgbody=MsgBody}, Resp) ->
    <<packetize(stline, Resp)/binary,
      packetize(headers, Resp),
      MsgBody/binary>>.


packetize(stline, #response{version=Ver, stcode=Code, reason=Reason}=Resp) ->
    list_to_binary(Ver ++ " " ++ Code ++ " " ++ Reason ++ "\r\n");

packetize(headers, #response{hdrs=Hs}) ->
    packetize(Hs, []).

packetize(msgbody, #response{msgbody=MsgBody}) ->
    MsgBody.


packetize([], Acc) -> binary:list_to_bin( lists:reverse( Acc ));
packetize([{K,V} | Hs], Acc) ->
    H = <<atom_to_binary(K)/binary,list_to_binary(":" ++ V ++ "\r\n")/binary>>
    packetize( header, Hs, [H | Acc] ).


reason("100") -> "Continue";
reason("101") -> "Switching Protocols";
reason("200") -> "OK";
reason("201") -> "Created";
reason("202") -> "Accepted";
reason("203") -> "Non-Authoritative Information";
reason("204") -> "No Content";
reason("205") -> "Reset Content";
reason("206") -> "Partial Content";
reason("300") -> "Multiple Choices";
reason("301") -> "Moved Permanently";
reason("302") -> "Found";
reason("303") -> "See Other";
reason("304") -> "Not Modified";
reason("305") -> "Use Proxy";
reason("307") -> "Temporary Redirect";
reason("400") -> "Bad Request";
reason("401") -> "Unauthorized";
reason("402") -> "Payment Required";
reason("403") -> "Forbidden";
reason("404") -> "Not Found";
reason("405") -> "Method Not Allowed";
reason("406") -> "Not Acceptable";
reason("407") -> "Proxy Authentication Required";
reason("408") -> "Request Time-out";
reason("409") -> "Conflict";
reason("410") -> "Gone";
reason("411") -> "Length Required";
reason("412") -> "Precondition Failed";
reason("413") -> "Request Entity Too Large";
reason("414") -> "Request-URI Too Large";
reason("415") -> "Unsupported Media Type";
reason("416") -> "Requested range not satisfiable";
reason("417") -> "Expectation Failed";
reason("500") -> "Internal Server Error";
reason("501") -> "Not Implemented";
reason("502") -> "Bad Gateway";
reason("503") -> "Service Unavailable";
reason("504") -> "Gateway Time-out";
reason("505") -> "HTTP Version not supported".
