{ application, http,
    [ {description,  "Netscale web server and related tools" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [nhttp_sup]},
      {modules,      [nhttp_app]},
      {mod,          {nhttp_app,[]}},
      {applications, [kernel, stdlib]},
      {env,          [
            % Name of the root supervisor process for this application.
            { supname, nhttp_sup},
            % Port number to bind and listen for client http connection.
            { listenport, 80},
            % Listen options.
            { listenopts,
                [ binary,
                  {backlog, 1000},
                  {packet, http},
                  {active, once}
                ]},
            % Accept options
            { acceptopts, [] },
            % Simultaneous connections to maintain. As many procs will be
            % accepting for new connections.
            { n_connections, 1000 },
            % Childspecs for nhttp_sup process.
            { nhttp_sup_childspecs,
                [{ child_nhttpd,
                     {gen_server, start_link, 
                         [ {global,proc_nhttpd}, nhttpd, [], [] ]},
                     permanent,
                     1000,
                     worker,
                     [nhttpd]
                  }] }
            ]}
    ]
}.

% vim: set filetype=erlang:
