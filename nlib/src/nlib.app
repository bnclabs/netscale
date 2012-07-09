{ application, nlib,
    [ {description,  "Netscale libraries" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [nlib]},
      {modules,      [nutil,ndist,nfile]},
      {applications, [kernel, stdlib]},
      {env,          []}
    ]
}.
