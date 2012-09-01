{ application, nhttp,
    [ {description,  "Netscale web server and related tools" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [nhttp_sup,nhttpd]},
      {modules,      [nhttp_app,nhttpd,nhttps,nhttpc]},
      {mod,          {nhttp_app,[]}},
      {applications, [kernel, stdlib]},
      {env,          [
            % Name of the root supervisor process for this application.
            { supname, nhttp_sup},
            % Simultaneous connections to maintain. As many procs will be
            % accepting for new connections.
            { n_connections, 1000 },
            % Port number to bind and listen for client http connection.
            { listenport, 8080 },
            % Listen options.
            { listenopts,
                [ binary,
                  {reuseaddr, true},% Development only !!
                  {backlog, 1000},
                  {packet, 0},
                  {active, once}    % Dont change this !!
                ]},
            % Accept options.
            { acceptopts, [] },
            % Timeout for inactive connection in milliseconds, def: 1 hour.
            { inactive_timeout, 3600000 },
            % Childspecs for nhttp_sup process.
            { nhttp_sup_childspecs,
                [{ nhttpd,
                     {gen_server, start_link,
                        [ {global,nhttpd}, nhttpd, [], [] ]},
                     permanent,
                     1000,
                     worker,
                     [nhttpd]
                  }] }
            ]}
    ]
}.

% vim: set filetype=erlang:
