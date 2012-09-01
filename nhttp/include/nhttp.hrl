-define( APPNAME, nhttp ).

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
-record( request, {
                  }).

%% @doc: Response record.
-record( response, {
                   }).
