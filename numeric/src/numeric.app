{ application, numeric,
    [ {description,  "Netscale's numeric algorithms" },
      {id,           "netscale"},
      {vsn,          "0.1"},
      {registered,   [numeric_sup, genprime]},
      {modules,      [num_app,num,prime]},
      {mod,          {num_app,[]}},
      {applications, [kernel, stdlib]},
      {env,          [
            % Name of root supervisor process.
            {supname, numeric_sup},
            % Number of DETS files to maintain for prime numbers.
            {num_primefiles, 1000},
            % Number of DETS files to maintain for factorials.
            {num_factorfiles, 1000},
            % Supervisor child specs.
            {numeric_sup_childspecs,
              [{ child_prime, 
                 {gen_server, start_link, 
                     [{global,proc_prime}, prime, [], []]},
                 permanent,
                 5000,
                 worker,
                 [prime]
               }]
            }
         ]}
    ]
}.

% vim: set filetype=erlang:
