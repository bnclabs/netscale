{ application, numeric,
    [ {description,  "Netscale's numeric algorithms" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [numeric_sup, genprime]},
      {modules,      [num_app,num,prime]},
      {mod,          {num_app,[]}},
      {applications, [kernel, stdlib]},
      {env,          [ {supname, numeric_sup},
                       {num_primefiles, 1000},
                       {num_factorfiles, 1000},
                       {childspecs,
                         [{ prime, 
                            {gen_server, start_link, 
                                [{global,genprime}, prime, [], []]},
                            permanent,
                            5000,
                            worker,
                            [prime]
                          }]
                       }
                     ]}
    ]
}.
