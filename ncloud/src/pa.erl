%% This module provides `gen_server` interface to python port running
%% pluggdapps web framework. A corresponding interface module in python can be
%% found under `pluggdapps.erlport`.

-module(pa).
-behaviour(gen_server).

-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "ncloud/include/pa.hrl" ).

%% gen_server behaviour.
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3 ]).

-define( MAXR, 10 ).    % Number of restarts,
-define( MAXT, 5 ).     % in number of seconds.


%%---- Callbacks for gen_server behaviour

init([gen_server | _Args]) ->
    process_flag( trap_exit, true ),
    PortArgs = application:get_env( paport_args ),
    NumPorts = application:get_env( num_paports ),
    Children = map( fun(_) -> spawn_link( paport, handleport, PortArgs ) end,
                    lists:seq(1,num_paports) ),
    {ok, #pastate{children=Children}}.

%%-- Calls

handle_call(_Req, _From, State) ->
    {noreply, State}.


%%-- Casts

handle_cast(_, _State) ->
    erlang:error("Unknown cast call").


%%-- Infos

handle_info( timeout, _State ) ->
    ok;

handle_info({Port, {exit_status, Status}}, _State) ->
    %% Port has exited. Check whether the port is part of the pool and restart
    %% them if required.

handle_info(_, _State) ->
    erlang:error("Unknown info call").


%%-- terminate

terminate(normal, State) ->
    ok;
terminate(shutdown, State) ->           % Supervisor shutdown
    ok;
terminate({shutdown, _Reason}, State) ->
    ok;
terminate(_Reason, State) ->            % Probably an error
    ok.


code_change(_A, _B, _C) ->
    erlang:error("Code change function not implemented.").


%% Local function

%% paport server reference.
server_name() ->
    {ok, Spec} = application:get_env(?APPNAME, childspecs),
    {_M, _F, [A | _As]} = element(2, proplists:lookup(prime, Spec)),
    A.

dispatch_to_pool( Command, Args, [RPid], WaitQ ) ->
    RPid ! {self(), {force, Command, Args}},
    case receive {RPid, Bool} -> Bool end of
        true -> {[], [RPid | WaitQ]}
    end;
dispatch_to_pool( Command, Args, [RPid | ReadyQ], WaitQ ) ->
    RPid ! {self(), {Command, Args}},
    case receive {RPid, Bool} -> Bool end of
        true -> {ReadQ, [RPid | WaitQ]};
        false -> dispatch_to_pool( Command, Args, ReadyQ, [RPid | WaitQ] )
    end.

