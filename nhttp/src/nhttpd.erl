-module(nhttpd).
-behaviour(gen_server).

-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "nhttp/include/nhttp.hrl" ).

%% gen_server behaviour.
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3 ]).

doaccept( Listen ) ->
    {ok, Socket} = gen_tcp:accept( Listen ),
    gen_server:start_link( Socket );


%%---- Callbacks for gen_server behaviour

init( Args ) ->
    Options = read_configfile( Args ),
    {port, Port} = proplists:lookup( port, Options ),
    {listenopts, ListenOpts} = proplists:lookup( listenopts, Options ),
    case gen_tcp:listen( Port, ListenOpts ) of
        {ok, Listen} ->
            {ok, Socket} = gen_tcp:accept( Listen ),
            loop( Socket );
        {error, Reason} ->
            error_logger:error_msg( "nhttpd listen failure on port", [Port] )
    end.


%%-- Calls

handle_call(_Req, _From, State) ->
    {noreply, State}.


%%-- Casts

handle_cast(Req, _State) ->
    error_logger:error_msg( "Cast request ~s not supported", [Req] ).


%%-- Infos

handle_info(timeout, _State) ->
    error_logger:error_msg( "Timed out !!" );

handle_info(Info, _State) ->
    error_logger:error_msg( "Unable to handle info ~s ~n", [Info] ).


%%-- terminate

terminate(normal, State) ->
    stop_servers( State#httpd.spids ),
    ok;
terminate(shutdown, State) ->
    stop_servers( State#httpd.spids ),
    ok;
terminate({shutdown, _Reason}, State) ->
    stop_servers( State#httpd.spids ),
    ok;
terminate(_Reason, State) ->        % Error
    stop_servers( State#httpd.spids ),
    ok.


code_change(_A, _B, _C) ->
    erlang:error("Code change function not implemented.").


%%---- local functions

read_configfile(Args) ->
    {configfile, ConfigFile} = proplists:lookup( configfile, Args ),
    ConfigFileA = filename:join([
                    filename:dirname( code:lib_dir( ?APPNAME )),
                    ConfigFile ]),
    [Options] = file:consult( ConfigFileA ),
    Options.


loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n" ,[Bin]),
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n" ,[Str]),
            Reply = lib_misc:string2value(Str),
            io:format("Server replying = ~p~n" ,[Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed ~n")
    end.

