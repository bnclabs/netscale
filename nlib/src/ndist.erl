-module(ndist).

-export([ appstart/1, appstart/2, appstop/1, appstop/2 ]).

% TODO : 
%   Error logger for appstart() and appstop().

% Start the given application on all nodes.
appstart( [], _AppName ) -> ok;
appstart( [Node | Nodes], AppName ) ->
    case rpc:call( Node, application, start, [AppName] ) of
        {error, Reason} -> erlang:display(Reason);
        ok -> ok
    end,
    appstart( Nodes, AppName ).

appstart( AppName  ) -> appstart( net_adm:host_file(), AppName ).


% Stop the given application on all nodes.
appstop( [], _AppName ) -> ok;
appstop( [Node | Nodes], AppName ) ->
    case rpc:call( Node, application, stop, [AppName] ) of
        {error, Reason} -> erlang:display(Reason);
        ok -> ok
    end,
    appstop( Nodes, AppName ).

appstop( AppName  ) -> appstop( net_adm:host_file(), AppName ).
