{ application, nlib,
    [ {description,  "Netscale libraries" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [nlib]},
      {modules,      [nutil,ndist]},
      {applications, [kernel, stdlib]},
      {env,          []}
    ]
}.
