{ application, ncloud,
    [ {description,  "Netscale libraries" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [ncloud_sup]},
      {modules,      [ncloud_app,ndist,nutil,pa,paport,wpool,run_eunit]},
      {mod,          {ncloud_app,[]}},
      {applications, [kernel, stdlib]},
      {env,          [
            % Name of root supervisor process.
            {supname, ncloud_sup},
            % Whether to periodically check for modified modules (for
            % development).
            {reload, false},
            % number of pluggdapps port to open.
            {num_paports, 2},
            % Entry point for pluggdapps via erlang's netscale.
            {paport_name, {spawn_executable, "erlmain.py"}},
            % Pluggdapps port settings. Can be overriden in the config file.
            {paport_sett,
              [ {packet, 4},
                {args, 
                  [ "--packet", "4",
                    "--config-ini", "/home/pratap/dev/netscale/master.ini",
                    "--virtenv", "pa-env/lib/python3.2/site-packages",
                    "--nouse_stdio"
                  ]},
                exit_status,
                eof,
                nouse_stdio
              ]},
            % Supervisor child specs.
            {ncloud_sup_childspecs,
              [{ child_pa,
                 {gen_server, start_link, 
                     [{global,proc_pa}, pa, [], []]},
                 permanent,
                 1000,
                 worker,
                 [pa]
               }] }
         ]}
    ]
}.

% vim: set filetype=erlang:
