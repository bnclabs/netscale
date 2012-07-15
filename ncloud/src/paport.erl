-module(paport).

-export([ openport/2 ]).

%% Spawn this ! Connected process for pluggdapps port.
openport( Parent, [PortName, PortSettings] ) ->
    Port = open_port( PortName, PortSettings ),
    ?MODULE:requestport( Parent, Port ).


%% Marshal request from gen_server to pluggdapps.
requestport( Parent, Port ) ->
    {Ref, Res} = receive
                    { Parent, {force, Ref, Method, Args, KWArgs} } ->
                        Data = term_to_binary( {req, Method, Args, KWArgs} ),
                        {Ref, port_command( Port, Data )};
                    { Parent, {Ref, Method, Args, KWArgs} } ->
                        Data = term_to_binary( {req, Method, Args, KWArgs} ),
                        {Ref, port_command( Port, Data, [nosuspend] )}
                 end,
    case Res of
        false ->
            Parent ! {self(), false}
            ?MODULE:requestport( Parent, Port );
        true ->
            Parent ! {self(), true}
            ?MODULE:respondport( Parent, Port, Ref )
    end.


%% Collect the response back from pluggdapps. Also handle requests from
%% pluggdapps. All request from pluggdapps must be handle in the context of an
%% original request from gen_server.
respondport( Parent, Port, Ref ) ->
    receive
        { Port, {data, Data}} ->
            case binary_to_term( Data ) of
                { resp, {ok, Response} } ->
                    Parent ! {Ref, Response},
                    ?MODULE:requestport( Parent, Port );
                { req, Method, Args, KWArgs } ->
                    port_command( Port, docommand(Method, Args, KWArgs, Ref) ),
                    ?MODULE:respondport( Parent, Port, Ref )
            end
    end.

docommand( Method, Args, KWArgs, Ref ) ->
    term_to_binary( {resp, ok} ).
