{ application, ncloud,
    [ {description,  "Netscale libraries" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [ncloud_sup]},
      {modules,      [ncloud_app,ndist,nutil,pa,run_eunit,wpool]},
      {applications, [kernel, stdlib]},
      {env,          [ {supname, ncloud_sup},
                       {num_paports, 1}
                       {paport_args,
                           [ {spawn_executable,
                                "/home/pratap/dev/netscale/pluggdapps/pluggdapps/erlport.py"},
                             [ {packet, 4},
                               {args, ["--packet", "4"]},
                               exit_status,
                               use_stdio,
                             ]
                           ]
                       },
                       {childspecs,
                         [{ paport, 
                            {gen_server, start_link, [{global,paport}, pa, [], []]},
                            permanent,
                            1000,
                            worker,
                            [pa]
                          }]
                       }
                     ]}
    ]
}.
