-module(paport).

-export([ launchport/2, requestport/2, respondport/3 ]).

%% Spawn this ! Connected process for pluggdapps port.
launchport( Parent, {PortName, PortSettings} ) ->
    Port = open_port( PortName, PortSettings ),
    error_logger:info_msg( "Launched pa-port : ~p, connected process : ~p ~n",
                           [Port, self()] ),
    ?MODULE:requestport( Parent, Port ).


% Request methods from erlang to python,
%      exit_port
%      close_io
%      reverseback
%      profileback
%
% Request methods both ways
%      loopback 
%      apply
%      query_plugin
%      plugin_attribute
%      plugin_method

%% Marshal request from gen_server to pluggdapps.
requestport( Parent, Port ) ->
    receive
        { Parent, {From, Method, Args, KWArgs} } ->
            Data = term_to_binary( {req, Method, Args, KWArgs} ),
            % io:format( "~p ~n", [Data] ),
            port_command( Port, Data ),
            ?MODULE:respondport( Parent, Port, From );

        {Parent, get_portid} ->
            Parent ! {self(), portid, Port},
            requestport( Parent, Port );

        {Port, {exit_status, Status}} ->
            error_logger:error_msg(
                "Pluggdapps port ~p exited with ~p ~n", [Port, Status] ),
            erlang:exit({port_exit, Status});

        {Port, eof} ->
            error_logger:error_msg("Pluggdapps port ~p closed fd ~n", [Port]),
            erlang:exit({port_exit, eof})
    end.


%% Collect the response back from pluggdapps. Also handle requests from
%% pluggdapps. All request from pluggdapps must be handle in the context of an
%% original request from gen_server.
respondport( Parent, Port, From ) ->
    receive
        { Port, {data, Data}} ->
            % io:format( "~p ~n", [Data] ),
            case binary_to_term( list_to_binary(Data) ) of
                { resp, Response } ->
                    Parent ! {self(), From, {resp, Response}},
                    ?MODULE:requestport( Parent, Port );
                { req, Method, Args, KWArgs } ->
                    NResp = docommand(Method, Args, KWArgs, From),
                    port_command( Port, term_to_binary({resp, NResp}) ),
                    ?MODULE:respondport( Parent, Port, From );
                { post, Method, Args, KWArgs } ->
                    docommand(Method, Args, KWArgs, From),
                    ?MODULE:respondport( Parent, Port, From )
            end;

        {Parent, get_portid} ->
            Parent ! {self(), portid, Port},
            respondport( Parent, Port, From );

        {Port, {exit_status, Status}} ->
            error_logger:error_msg(
                "Pluggdapps port ~p exited with ~p ~n", [Port, Status] ),
            erlang:exit({port_exit, Status});

        {Port, eof} ->
            error_logger:error_msg("Pluggdapps port ~p closed fd ~n", [Port]),
            erlang:exit({port_exit, eof})
    end.


%%---- Local functions

docommand( loopback, Args, KWArgs, _From ) ->
    {Args, KWArgs};

docommand( logerror, [Fmt, Vals], _KWArgs, _From ) ->
    FmtS = pa:nstring_to_data(Fmt),
    apply( error_logger, error_msg, [FmtS, Vals] );

docommand( loginfo, [Fmt, Vals], _KWArgs, _From ) ->
    FmtS = pa:nstring_to_data(Fmt),
    apply( error_logger, info_msg, [FmtS, Vals] );

docommand( logwarn, [Fmt, Vals], _KWArgs, _From ) ->
    FmtS = pa:nstring_to_data(Fmt),
    apply( error_logger, warning_msg, [FmtS, Vals] ).

%%docommand( apply, Args, KWArgs, From ) ->
%%    term_to_binary( {resp, ok} );
%%
%%docommand( query_plugin, Args, KWArgs, From ) ->
%%    term_to_binary( {resp, ok} );
%%
%%docommand( plugin_attribute, Args, KWArgs, From ) ->
%%    term_to_binary( {resp, ok} );
%%
%%docommand( plugin_method, Args, KWArgs, From ) ->
%%    term_to_binary( {resp, ok} ).
