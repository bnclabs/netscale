-include_lib( "ncloud/include/ncloud.hrl" ).

-record( pastate,
            { numports,         % Number of pluggdapps port to spawn
              portargs,         % Arguments to open_port/2
              childports=[],    % proplist of connected process and ports
              readyq=[],        % list of connected process waiting for request
              requestq=[]       % list of request methods waiting to be handled
            }).
