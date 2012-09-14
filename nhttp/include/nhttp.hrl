-define( APPNAME, nhttp ).

%% @doc: Regular expression strings for HTTP BNF (Refer RFC2616.txt).
-define( RE_LWS,    <<"\r\n[ \t]+">> ).
-define( RE_TOKEN,  <<"[^()<>@,;:\\\\\"/\\[\\]\?={} \t\\x0-\x1f\x7f-\xff]+">> ).
-define( RE_HEADER, 
         <<"([ \t]*)(",?RE_TOKEN/binary,"):([^\r\n]+)(\r\n)?">>
         % [_, _, Field, _, Value, _]
       ).

-define( RE_DATETIME_RFC1123,
         <<"(...), (..) (...) (....) (..):(..):(..) GMT.*">>
         % [ _, Day, Date, Month, Year, Hour, Min, Sec ]
       ).
-define( RE_DATETIME_RFC1036,
         <<"([a-zA-Z]+), (..)-(...)-(..) (..):(..):(..) GMT.*">>
         % [ _, Day, Date, Month, Year, Hour, Min, Sec ]
       ).
-define( RE_DATETIME_ASCTIME,
         <<"(...) (...) (..) (..):(..):(..) (....).*">>
         % [ _, Day, Month, Date, Hour, Min, Sec, Year ]
       ).


%% @doc: gen_server state for nhttpd.
-record( nhttpd, { n_conn,     % Number of simultaneous connections.
                   port,       % Port to listen for http connections.
                   lopts,      % Listen options.
                   aopts,      % Accept options while accepting new connections.
                   lsock,      % Listening socket for new connections.
                   conns=[],   % List of connected servers procs.
                   daemon      % Daemon pid waiting for a new connection.
                 }).

%% @doc: gen_server state for nhttps.
-record( nhttps, { lsock,       % Listening socket for new connections.
                   socket,      % Connected / accepted socket.
                   conntimeout, % Timeout for inactive connection.
                   request,     % Request record.
                   response     % Response record.
                 }).

%% @doc: Request record.
-record( request, { % State value for parsing this request.
                    %  req_start, hdr_start, hdr_cont, body, req_end, invalid
                    state=req_start,
                    method,     % HTTP Method atom.
                    uri,        % URI Record.
                    version,    % HTTP Version tuple.
                    hdrs=[],    % List of cured http header field/value pairs.
                    msgbody,    % Binary string of request entity-body.
                                %   For chunked T-E it must have 'chunked'
                    entbody=[], % List of entity chunks, last chunk will be
                                %   empty binary string.
                    leftover= <<>> % Entire request may not be available as a
                                   %   single block of binary data.
                  }).

%% @doc: Response record.
-record( response, { % State value while composing a response.
                     %  stline
                     state=stline,
                     version,   % HTTP Version supported by nhttp server
                     stcode,    % Response status code 
                     reason,    % Reason.
                     hdrs=[],   % List of Response headers.
                     msgbody,   % Binary string of response entity-body.
                                %   For chunked T-E it must have 'chunked'
                   }).

%% @doc: URI record.
-record( httpuri, { type,        % Type of uri :
                                 %      * | abspath | absolute | authority
                    scheme=none, % URI scheme, either http or https.
                    user=none,   % User to be authenticated.
                    pass=none,   % Password to for authenticating user.
                    host=none,   % Hostname.
                    port=none,   % Port at which the server is listening.
                    path=none,   % URL path.
                    quer=none,   % Query string.
                    frag=none    % URL Fragment.
              }).


