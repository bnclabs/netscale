%% This module provides `gen_server` interface to python port running
%% pluggdapps web framework. A corresponding interface module in python can be
%% found under `pluggdapps.erlport` and `pluggdapps.ncloud`.
%%
%% * Request and Response between Erlang and Python is handled asynchronously.
%% * Request and Response betwenn Python and Erlang is handled synchronously.
%%   Since, the later is always in the context of the former.

-module(pa).
-behaviour(gen_server).

-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "ncloud/include/pa.hrl" ).

%% gen_server behaviour.
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3 ]).
%% Exported APIs
-export([ pa_dir/0, server_name/0,  getstate/0, exit_port/1,
          % Test interface
          loopback/1, reverseback/0, profileback/0,
          data_to_string/1, data_to_string/2, data_to_string/3,
          nstring_to_data/1 ]).
%% Non-APIs
-export([ profile/0 ]).

-define( MAXR, 10 ).    % Number of restarts,
-define( MAXT, 5 ).     % in number of seconds.


%%---- APIs

%% @doc : Package path for pluggdapps.
pa_dir() ->
    filename:join([
        filename:dirname( code:lib_dir( ?APPNAME )),
        "pluggdapps",
        "pluggdapps"
    ]).


%% @doc : `paports` server reference.
server_name() ->
    {ok, Spec} = application:get_env(?APPNAME, childspecs),
    {_M, _F, [A | _As]} = element(2, proplists:lookup(paport, Spec)),
    A.


%% @doc : Return `paports` server status.
getstate() ->
    gen_server:call( server_name(), getstate, infinity ).


%% @doc : Request port connected to `Pid` to exit normally.
exit_port(Pid) ->
    gen_server:call( server_name(), {exit_port, Pid}, infinity ).


%% @doc : loopback call to port. If successfully, the same term will be
%% returned back as,
%%      {ok, Term}
%% otherwise,
%%      {error, Reason}
loopback(Term) ->
    case gen_server:call( server_name(), {loopback, Term}, infinity ) of
        {ok, {[TermX], []}} -> {ok, TermX};
        {error, Reason} -> {error, Reason}
    end.


%% @doc : Request the port to perform loopback on its side. Similar to calling
%% loopback from this module. Used for testing purpose.
reverseback() ->
    gen_server:call( server_name(), reverseback, infinity ).
     
profileback() ->
    gen_server:call( server_name(), profileback, infinity ).
     

%% @doc : Erlang does not support native strings. Instead, term_to_binary/1
%% automagically encodes list of small integers (< 255) as string. Pluggdapps
%% port is intelligent enough understand this. On the other hand, pluggdapps
%% (python's) string data type is encoded as,
%%      { nstr, Encoding, Binary }
%% The following APIs act as the codec for this.
data_to_string(Data) -> data_to_string( Data, utf8, utf8 ).
data_to_string(Data, InEnc) -> data_to_string( Data, InEnc, utf8 ).
data_to_string(Data, InEnc, OutEnc) ->
    {nstr, OutEnc, unicode:characters_to_binary( Data, InEnc, OutEnc )}.
nstring_to_data({nstr, InEnc, Data}) -> unicode:characters_to_list(Data, InEnc).

%%---- Callbacks for gen_server behaviour

init(_Args) ->
    process_flag( trap_exit, true ),
    {ok, {Type, RelPath}} = application:get_env( paport_name ),
    {ok, PortSett} = application:get_env( paport_sett ),
    PortCmd = filename:join( pa_dir(), RelPath ),
    PortArgs = [self(), {{Type, PortCmd}, PortSett}],
    {ok, NumPorts} = application:get_env( num_paports ),

    %% Launch port-process
    Children = lists:map( 
                    fun(_) -> spawn_link(paport, launchport, PortArgs) end,
                    lists:seq(1,NumPorts) ),
    %% Make a property list out of connected process and its port.
    ChildPorts = get_portids( Children, [] ),

    {ok, #pastate{numports=NumPorts, portargs=PortArgs, 
                  childports=ChildPorts, readyq=Children}}.


%%-- Calls

handle_call(getstate, _From, State) ->
    {reply, State, State};

handle_call({exit_port, Pid}, From, State) ->
    NewState = dispatch_request( Pid, From, exit_port, [], [], State ),
    {reply, ok, NewState};

handle_call({loopback, Term}, From, State) ->
    NewState = dispatch_request( From, loopback, [Term], [], State ),
    {noreply, NewState};

handle_call(reverseback, From, State) ->
    NewState = dispatch_request( From, reverseback, [], [], State ),
    {noreply, NewState};

handle_call(profileback, From, State) ->
    NewState = dispatch_request( From, profileback, [], [], State ),
    {noreply, NewState};

handle_call(_Req, _From, State) ->
    {noreply, State}.


%%-- Casts

handle_cast(Req, _State) ->
    error_logger:error_msg( "Cast request ~s not supported", [Req] ).


%%-- Infos

handle_info( timeout, _State ) ->
    error_logger:error_msg( "Timed out !!" );

%% Asynchronous response
handle_info( {Pid, From, {resp, Response}}, #pastate{readyq=ReadyQ}=State ) ->
    gen_server:reply( From, Response ),
    NewState = dispatch_request( State#pastate{readyq=[Pid | ReadyQ]} ),
    {noreply, NewState};

%% Port has exited in a known manner. Restart them anyway.
handle_info({'EXIT', Pid, {port_exit, _Reason}}, State) ->
    NewState = restart_port( Pid, State ),
    {noreply, NewState};

%% Port has exited in an unknown manner. Restart them anyway.
handle_info({'EXIT', Pid, Reason}, State) ->
    NewState = restart_port( Pid, State ),
    error_logger:error_msg( 
        "Connected proc ~p failed with unknown reason ~p ~n", [Pid, Reason] ),
    {noreply, NewState};

handle_info(Info, _State) ->
    error_logger:error_msg( "Unable to handle info ~s ~n", [Info] ).


%%-- terminate

terminate(normal, #pastate{childports=ChildPorts}=State) ->
    stop_ports( ChildPorts, State ),
    ok;
terminate(shutdown, #pastate{childports=ChildPorts}=State) ->
    stop_ports( ChildPorts, State ),
    ok;
terminate({shutdown, _Reason}, #pastate{childports=ChildPorts}=State) ->
    stop_ports( ChildPorts, State ),
    ok;
terminate(_Reason, #pastate{childports=ChildPorts}=State) -> % error
    stop_ports( ChildPorts, State ),
    ok.


code_change(_A, _B, _C) ->
    erlang:error("Code change function not implemented.").


%%---- Local function

% @doc : dispatch request to a port waiting for request (in readyq). If no
% ports are in readyq, then queue the request.
dispatch_request(From, Method, Args, KWArgs,
                 #pastate{readyq=[], requestq=Reqs}=State) ->
    State#pastate{ requestq=[{From, Method, Args, KWArgs} | Reqs]};

dispatch_request(From, Method, Args, KWArgs, #pastate{readyq=ReadyQ}=State) ->
    {RemRQ, Pid} = nutil:init_last( ReadyQ ),
    Pid ! {self(), {From, Method, Args, KWArgs}},
    State#pastate{ readyq=RemRQ }.

% @doc : Directly send the request to the suggested Pid.
dispatch_request(Pid, From, Method, Args, KWArgs, State) ->
    Pid ! {self(), {From, Method, Args, KWArgs}},
    State.

% @doc : Check for requests in request-queue and pick one on FIFO basis. Send
% the request to a port waiting for request.
dispatch_request(#pastate{requestq=[]}=State) -> State;
dispatch_request(#pastate{requestq=Reqs}=State) ->
    { {From, Method, Args, KWArgs}, RemReqs } = nutil:init_last( Reqs ),
    dispatch_request( From, Method, Args, KWArgs,
                      State#pastate{requestq=RemReqs} ).


% @doc : Pid connected to port has exited. Check whether Pid is part of the 
% pool and cleanup the State.
restart_port( Pid, #pastate{ portargs=PortArgs, childports=ChildPorts, 
                             readyq=ReadyQ }=State) ->
    % Spawn new port application.
    NewPid = spawn_link( paport, launchport, PortArgs ),
    [{NewPid, NewPort}] = get_portids([NewPid], []),

    % Delete old port information from server state.
    ChildPorts1 = [ {NewPid, NewPort} | proplists:delete(Pid, ChildPorts) ],
    ReadyQ1 = [NewPid | lists:delete( Pid, ReadyQ )],
    State#pastate{ childports=ChildPorts1, readyq=ReadyQ1 }.


% @doc : Stop the children ports.
stop_ports([], _State) -> ok;
stop_ports([{Pid, _Port} | Children], State) ->
    dispatch_request( Pid, {none, none}, exit_port, [], [], State ),
    stop_ports(Children, State).


get_portids([], Acc) -> lists:reverse( Acc );
get_portids([Child | Children], Acc) ->
    Child ! { self(), get_portid },
    receive
        {Child, portid, Port} -> get_portids( Children, [{Child, Port} | Acc] )
    end.


%%---- Local test cases.

test_data() ->
    Tuple = { 10.2, 10000000000000.2, 0.00000000001, 
              <<10:4>>, <<1,2,3,5:3>>,
              10, 1000000, 1000000000000000000000000000000000000000,
              hello, 'dasfdksfaj!@#!@#!@',
              list_to_tuple( lists:seq( 1, 1000 )),
              [],
              data_to_string( "hello world" ),
              lists:seq( 1, 1000 ),
              <<1,2,3,4,5>>
            },
    [ 10.2, 10000000000000.2, 0.00000000001, 
      <<10:4>>, <<1,2,3,5:3>>,
      10, 1000000, 1000000000000000000000000000000000000000,
      hello, 'dasfdksfaj!@#!@#!@', Tuple, 
      <<1,2,3,4>>, lists:seq(1, 1000) ].
    

profile() ->
    Int  = {smallint, 10, 10000},
    BInt = {bigint, 100000, 10000},
    LInt = {largeint,
            10000000000000000000000000000000000000000000000000000000000000000 *
            10000000000000000000000000000000000000000000000000000000000000000 *
            10000000000000000000000000000000000000000000000000000000000000000 *
            10000000000000000000000000000000000000000000000000000000000000000 *
            10000000000000000000000000000000000000000000000000000000000000000,
            10000},
    Float = {float, 10.2, 10000},
    BitS  = {bitstring, <<1,2,3,4,5:3>>, 10000},
    Atom  = {atom, 'asdfkj123 !@#!@#', 10000},
    Tuple = {tuple, {Int, LInt, BInt, Float, BitS, Atom }, 10000},
    LTuple = {largetuple, lists:duplicate(100, Tuple), 100},
    List  = {list, [Int, LInt, BInt, Float, BitS, Atom ], 10000},
    Bin   = {binary, term_to_binary( lists:seq(1, 1000)), 10000},
    LBin  = {largebinary, term_to_binary( lists:seq(1, 1000000)), 10},
    Data  = {data, test_data(),100},

    ProfileFn = fun({Name, D, N}) ->
                    Dups  = lists:duplicate(N, D),
                    {T,_} = timer:tc( lists, map, [fun loopback/1, Dups]),
                    io:format( "Time taken to loop back ~p is ~p uS~n",
                               [Name, T/N ] )
                end,
    lists:map( ProfileFn, [Int, BInt,LInt, Float, BitS, Atom, Tuple, LTuple,
                           List, Bin, LBin, Data] ).


-ifdef(EUNIT).

getstate_test_() ->
    State = getstate(),
    [ ?_assertEqual(length(State#pastate.childports), State#pastate.numports),
      ?_assertEqual(length(State#pastate.readyq), State#pastate.numports) ].


loopback_test_() ->
    Ref = test_data(),
    RefStr = "hello world",
    {ok, {Data, DataStr}} = loopback( {Ref, data_to_string(RefStr)} ),
    [ ?_assertEqual( Ref, Data ),
      ?_assertEqual( RefStr, nstring_to_data( DataStr )) ].

-endif.
