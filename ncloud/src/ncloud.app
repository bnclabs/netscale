{ application, ncloud,
    [ {description,  "Netscale libraries" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [ncloud_sup]},
      {modules,      [ncloud_app,ndist,nutil,pa,paport,wpool,run_eunit]},
      {mod,          {ncloud_app,[]}},
      {applications, [kernel, stdlib]},
      {env,          [ {supname, ncloud_sup},
                       {reload, false},
                       {pa_inifile, "/home/pratap/dev/netscale/master.ini"},
                       {num_paports, 2},
                       {paport_name, 
                         {spawn_executable, "erlport.py"}},
                       {paport_sett,
                         [ {packet, 4},
                           {args, 
                             [ "--packet", "4",
                               "--virtenv", "pa-env/lib/python3.2/site-packages",
                               "--nouse_stdio"
                             ]},
                           exit_status,
                           eof,
                           nouse_stdio
                         ]},
                       {childspecs,
                         [{ paport,
                            {gen_server, start_link, 
                                [{global,paports}, pa, [], []]},
                            permanent,
                            1000,
                            worker,
                            [pa]
                          }] }
                     ]}
    ]
}.

%% vim : set filetype=erlang
