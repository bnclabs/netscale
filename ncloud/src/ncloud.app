{ application, ncloud,
    [ {description,  "Netscale libraries" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [ncloud]},
      {modules,      [nutil,ndist,nfile]},
      {applications, [kernel, stdlib]},
      {env,          []}
    ]
}.
