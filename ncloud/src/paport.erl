-module(paport).

-export([ launchport/2, requestport/3, respondport/3 ]).

%% Spawn this ! Connected process for pluggdapps port.
launchport( Parent, {PortName, PortSettings} ) ->
    Port = open_port( PortName, PortSettings ),
    error_logger:info_msg( "Launched pa-port : ~p, connected process : ~p ~n",
                           [Port, self()] ),
    ?MODULE:requestport( Parent, Port, none ).


% Request methods from erlang to python,
%      exit_port
%      close_io
%      reverseback
%      profileback
% Request methods from python to erlang
%      logerror
%      loginfo
%      logwarn
% Request methods both ways
%      loopback 
%      pyapply
%      query_plugin
%      plugin_attribute
%      plugin_method

%% Marshal request from gen_server to pluggdapps.
requestport( Parent, Port, none ) ->
    receive
        {Parent, {From, Method, Args, KWArgs}} ->
            port_command( Port, term_to_binary({req, Method, Args, KWArgs}) ),
            ?MODULE:respondport( Parent, Port, From );

        {Parent, get_portid} ->
            Parent ! {self(), portid, Port},
            ?MODULE:requestport( Parent, Port, none );

        {Port, {data, Data}} ->
            PortMsg = binary_to_term( list_to_binary(Data) ),
            portmessage(
                {Port, PortMsg}, ?MODULE, requestport, [Parent, Port, none]);

        OtherMsg -> 
            portmessage(
                OtherMsg, ?MODULE, requestport, [Parent, Port, none] )
    end.


%% Collect the response back from pluggdapps. Also handle requests from
%% pluggdapps.
respondport( Parent, Port, From ) ->
    receive
        {Port, {data, Data}} ->
            PortMsg = binary_to_term( list_to_binary(Data) ),
            portmessage( 
                {Port, PortMsg}, ?MODULE, respondport, [Parent,Port,From]);
        OtherMsg -> 
            portmessage(
                OtherMsg, ?MODULE, respondport, [Parent, Port, From] )
    end.


%% Handle pluggdapps port messages
portmessage({Port, {exit_status, Status}}, _Mod, _Fun, _Args) ->
    error_logger:error_msg(
            "Pluggdapps port ~p exited with ~p ~n", [Port, Status] ),
    erlang:exit({port_exit, Status});

portmessage({Port, eof}, _Mod, _Fun, _Args) ->
    error_logger:error_msg("Pluggdapps port ~p closed fd ~n", [Port]),
    erlang:exit({port_exit, eof});

portmessage({Port, {resp, _Resp}}, Mod, Fun, [Parent, Port, none]) ->
    error_logger:error_msg(
        "pa response message received while waiting for gen_server request" ),
    Mod:Fun( Parent, Port, none );

portmessage({Port, {resp, Response}}, _Mod, _Fun, [Parent, Port, From]) ->
    Parent ! {self(), From, {resp, Response}},
    ?MODULE:requestport( Parent, Port, none );

portmessage(
  {Port, {req, Method, Args, KWArgs}}, Mod, Fun, [Parent, Port, From]) ->
    NResp = docommand( Method, Args, KWArgs, From ),
    port_command( Port, term_to_binary({resp, NResp}) ),
    Mod:Fun( Parent, Port, From );

portmessage(
  {Port, {post, Method, Args, KWArgs}}, Mod, Fun, [Parent, Port, From]) ->
    docommand( Method, Args, KWArgs, none ),
    Mod:Fun( Parent, Port, From );

portmessage(Msg, Mod, Fun, [Parent, Port, From]) ->
    error_logger:error_msg(
        "Unable to handle marshalled messasge from pa. ~p ~n", [Msg] ),
    Mod:Fun( Parent, Port, From ).


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
