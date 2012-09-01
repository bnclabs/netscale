-module(nhttpd).
-behaviour(gen_server).

-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "nhttp/include/nhttp.hrl" ).

-define( DAEMON_NAME, {global, nhttpd} ).

%% gen_server behaviour.
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3 ]).
%% API Exports
-export([ getstate/0, daemon/0, conn_accepted/2 ]).

%% @doc: Internal server state for `nhttpd`.
getstate() ->
    gen_server:call( ?DAEMON_NAME, getstate ).

%% @doc: Only one `nhttps` process (the daemon) waits accepting a new connection
%% at a time. When a connection is accepted a fresh `nhttps` process should be
%% started.
daemon() ->
    gen_server:cast( ?DAEMON_NAME, daemon ).

%% @doc: `nhttps` server has accepted a new connection and informs the same
%% to the master-daemon.
conn_accepted( Pid, Socket ) ->
    gen_server:cast( ?DAEMON_NAME, {conn_accepted,Pid,Socket} ).

%%---- Callbacks for gen_server behaviour

init( _Args ) ->
    process_flag( trap_exit, true ),
    {ok, NConn} = application:get_env( ?APPNAME, n_connections ),
    {ok, Port} = application:get_env( ?APPNAME, listenport ),
    {ok, ListenOpts} = application:get_env( ?APPNAME, listenopts ),
    {ok, AcceptOpts} = application:get_env( ?APPNAME, acceptopts ),
    State = #nhttpd{ n_conn=NConn, port=Port, lopts=ListenOpts,
                     aopts=AcceptOpts },
    case gen_tcp:listen( Port, ListenOpts ) of
        {error, Reason} ->
            error_logger:error_msg( "nhttp listen error ~p~n", [Reason] ),
            {stop, Reason};
        {ok, LSock}     ->
            {ok, start_daemon( State, LSock )}
    end.


%%-- Calls

handle_call(getstate, _From, State) ->
    {reply, State, State};

handle_call(Req, From, State) ->
    error_logger:error_msg( "Unknown call ~p from ~p ~n", [Req, From] ),
    {noreply, State}.


%%-- Casts

handle_cast(daemon, State) ->
    {noreply, start_daemon( State )};

handle_cast({conn_accepted, Pid, Socket}, #nhttpd{conns=Conns}=State) ->
    NewState = State#nhttpd{ conns=[{Pid,Socket} | Conns] },
    {noreply, start_daemon( NewState )};

handle_cast(Req, State) ->
    error_logger:error_msg( "Cast request ~s not supported", [Req] ),
    {noreply, State}.


%%-- Infos

handle_info(timeout, State) ->
    stop_daemon( State ),
    {stop, "Timed out occured, stopping `nhttpd` daemon", State};

handle_info({'EXIT',Pid,_Reason}, State) ->
    if Pid == State#nhttpd.daemon -> {noreply, start_daemon(State)};
       true -> {noreply, State}
    end;

handle_info(Info, State) ->
    Msg = io_lib:format( "Unable to handle Info ~p, stopping daemon", [Info] ),
    stop_daemon( State ),
    {stop, Msg, State}.


%%-- terminate.
%%  Note that for any other reason than normal, shutdown, or {shutdown,Term}
%%  the gen_server is  assumed to terminate due to an error and an error 
%%  report is issued using error_logger:format/2.

terminate(normal, State) ->
    stop_daemon( State ),
    ok;
terminate(shutdown, State) ->
    stop_daemon( State ),
    ok;
terminate({shutdown, _Reason}, State) ->
    stop_daemon( State ),
    ok;
terminate(_Error, State) ->        % Error
    stop_daemon( State ),
    ok.


code_change(_A, _B, _C) ->
    error_logger:error_msg("Code change function not implemented.").


%%---- The daemon

start_daemon( State ) -> start_daemon( State, State#nhttpd.lsock ).

start_daemon( State, LSock ) ->
    {ok, Pid} = gen_server:start_link( nhttps, [LSock], [] ),
    nhttps:doaccept( Pid ),
    State#nhttpd{ lsock=LSock, daemon=Pid }.

stop_connections([]) -> ok;
stop_connections([{Pid,_Sock} | Conns]) ->
    exit(Pid, shutdown),
    stop_connections( Conns ).

stop_daemon(State) ->
    gen_tcp:close( State#nhttpd.lsock ),
    stop_connections( State#nhttpd.conns ).
