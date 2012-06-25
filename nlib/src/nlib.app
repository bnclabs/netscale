{ application, nlib,
    [ {description,  "Netscale libraries" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {modules,      [nutil]},
      {applications, [kernel, stdlib]},
      {env,          []}
    ]
}.
