{ application, http,
    [ {description,  "Netscale web server and related tools" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [nhttp_sup]},
      {modules,      [nhttp_app]},
      {mod,          {nhttp_app,[]}},
      {applications, [kernel, stdlib]},
      {env,          [
            % Name of the root supervisor process for this application
            {supname, nhttp_sup},

            % Child process that are to be started by root supervisor
            {childspecs,
              [{ nhttpd,
                   {gen_server, start_link, 
                       [ {global,nhttpd},
                         nhttpd,
                         [ {configfile, "_config/nhttpd.config"} ],
                         [] ]},
                   permanent,
                   1000,
                   worker,
                   [pa]
                }] }
            ]}
    ]
}.

%% vim : set filetype=erlang %%
