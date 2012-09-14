-module(nhttps).
-behaviour(gen_server).

-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "nhttp/include/nhttp.hrl" ).

%% gen_server behaviour.
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3 ]).
%% API Exports
-export([ doaccept/1 ]).

doaccept( ServerRef ) ->
    gen_server:cast(ServerRef, accept).

%%---- Callbacks for gen_server behaviour

init( [LSock] ) ->
    process_flag( trap_exit, true ),
    {ok, ConnTimeout} = application:get_env( ?APPNAME, inactive_timeout ),
    State = #nhttps{request=#request{}, lsock=LSock, conntimeout=ConnTimeout},
    {ok, State}.


%%-- Calls

handle_call(Req, From, State) ->
    ?EMSG( "Unknown call ~p from ~p ~n", [Req, From] ),
    {noreply, State, State#nhttps.conntimeout}.


%%-- Casts

handle_cast(accept, #nhttps{lsock=LSock}=State) ->
    case gen_tcp:accept(LSock) of
        {error, Reason} -> 
            {stop, Reason, State};
        {ok, Socket} -> 
            NewState = State#nhttps{ socket=Socket },
            nhttpd:conn_accepted( self(), Socket ),
            {noreply, NewState, State#nhttps.conntimeout}
    end;

handle_cast(Req, State) ->
    stop_connection( State ),
    {stop, Req, State}.


%%-- Infos

handle_info(timeout, State) ->
    stop_connection( State ),
    {stop, "Timed out occured closing connection", State};

handle_info({tcp, Socket, Data}, State) ->
    NewState = dorequest( State, Data ),
    % TODO : Check for connection close and close connection after response is
    % sent.
    inet:setopts( Socket, [{active, once}] ),
    {noreply, NewState, NewState#nhttps.conntimeout};

handle_info({tcp_closed, Socket}, #nhttps{socket=Socket}=State) ->
    stop_connection( State ),
    nhttp_req:parse_req( State, closed ),
    % TODO : Check for connection close and close connection after response is
    % sent.
    {stop, "Remote connection closed", State};

handle_info({tcp_error, Socket, _Reason}, #nhttps{socket=Socket}=State) ->
    stop_connection( State ),
    % TODO : Error logging here.
    {stop, "Tcp error", State};

handle_info(Info, State) ->
    stop_connection( State ),
    Msg = io_lib:format( "Unable to handle Info ~p, stopping conn.", [Info] ),
    {stop, Msg, State}.


%%-- terminate.
%%  Note that for any other reason than normal, shutdown, or {shutdown,Term}
%%  the gen_server is  assumed to terminate due to an error and an error 
%%  report is issued using error_logger:format/2.

terminate(normal, State) ->
    stop_connection( State ),
    ok;
terminate(shutdown, State) ->
    stop_connection( State ),
    ok;
terminate({shutdown, _Reason}, State) ->
    stop_connection( State ),
    ok;
terminate(_Error, State) ->        % Error
    stop_connection( State ),
    ok.


code_change(_A, _B, _C) ->
    erlang:error("Code change function not implemented.").


%%---- The daemon

stop_connection( #nhttps{socket=Socket}=State ) ->
    if Socket =/= undefined -> gen_tcp:close( State#nhttps.socket ), ok;
       true -> ok
    end.

dorequest( #nhttps{request=Req, socket=Sock}=State, Data ) ->
    try nhttp_req:parse_request( Req, Data ) of
        #request{state=req_end}=FullReq -> 
            State#nhttps{request=#request{leftover=FullReq#request.leftover}};
        ContReq -> 
            State#nhttps{request=ContReq}
    catch
        throw:{status, Code, Body} ->
            nhttp_resp:start_response(Sock, Code, []),
            nhttp_resp:send_response(Sock, [], Body)
            State#nhttps{request=#request{}};
        throw:{status, Code, Body, FailReq} ->
            State#nhttps{request=#request{leftover=FailReq#request.leftover}};
        _ ->
            State
    end.
