% Prime numbers are computed and cached inside dets files. Since it is not
% possible to cache a large set of prime numbers in a single file many files
% will be used with each file being a bucket of prime numbers in a range of
% 100 million, or so, numbers. Inside each file prime numbers will be keyed
% for a range of 100 numbers.

-module(prime).
-behaviour(gen_server).

%% Prime server APIs.
-export([ getstate/0, housekeep/0, nprimes/1, primesWithin/2, isprime/1,
          store_primes/2, sync/0, primes/1 ]).

%% gen_server behaviour.
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3 ]).

%% Suppress warnings.
-export([ filterprimes/3, filterprimes/2, checkprime/2 ]).

-include_lib( "numeric/include/prime.hrl" ).
-include_lib( "eunit/include/eunit.hrl" ).

%% APIs for prime number generation, caching and services

% Return server `State`.
getstate() ->
    gen_server:call( server_name(), getstate ).


% Server housekeeping activities. Like,
%   * dets tables could have gone fragmented, close them and re-open with 
%     `repair` option set to `force`.
housekeep() -> 
    gen_server:call( server_name(), housekeep, infinity ).


% Return a list of prime numbers between `Start` and `End`.
primesWithin(Start, End) ->
    Rc = gen_server:call( server_name(), {primeswithin, Start, End}, infinity ),
    Primes = case Rc of
                {_Tag, PrimeD, 0} ->
                    lists:flatten( lists:map( 
                        fun({_, Ps}) -> Ps end, lists:reverse( PrimeD )
                    ));
                {Tag, PrimeD, RcvN} ->
                    lists:sort( lists:flatten( lists:map(
                        fun({_, Ps}) -> Ps end, rcv( Tag, PrimeD, RcvN )
                    )))
            end,
    erlang:display( element(1, lists:split(400, Primes))),
    erlang:display( element(1, lists:split(400, lists:reverse(Primes)))),
    lists:takewhile( fun(N) -> N =< End end, 
                     lists:dropwhile( fun(N) -> N < Start end, Primes )).


% Return a list of first `N` prime numbers.
nprimes(N) ->
    OrdPs = case gen_server:call( server_name(), {nprimes, N}, infinity ) of
                {_Tag, PrimeD, 0} -> lists:reverse( PrimeD );
                {Tag, PrimeD, RcvN} -> lists:sort( rcv( Tag, PrimeD, RcvN ))
            end,
    Primes = lists:flatten( lists:map( fun({_, Ps}) -> Ps end, OrdPs )),
    if 
        length(Primes) > N ->
            {Res, _} = lists:split( N, lists:flatten(Primes) ),
            Res;
        true->
            Primes
    end.


% Check whether `N` is prime or not.
isprime(N) ->
    Tag = gen_server:call( server_name(), {isprime, N}, infinity ),
    receive {Tag, Bool} -> Bool end.


% Store (cache) a list of prime numbers in a dets table under `Key`.
store_primes(Key, Primes) -> 
    gen_server:call( server_name(), {storeprimes, Key, Primes} ).


% flush data to DETS files.
sync() ->
    gen_server:cast( server_name(), sync ).


% Generate prime numbers in sequential manner starting from the first prime 2
% as long as predicate `Pred` returns False. Predicate has the following
% signature,
%   fun(N, Acc)
%   where N,   is a natural number to be checked for prime.
%         Acc, is a list of prime numbers computer so far (in reverse
%                  order).
primes(Pred) when is_function( Pred ) -> primesPred( 2, Pred, [] );

% Generate first N prime numbers in sequential manner.
primes(N) when is_integer( N ) -> primesN( 2, N, [] ).


%%---- Callbacks for gen_server behaviour

init(_Args) ->
    process_flag( trap_exit, true ),

    % Application configuration
    {ok, NumFiles} = application:get_env(num_primefiles),

    % Setup cache files for storing computed prime numbers
    Dir = filename:join([ code:priv_dir(?APPNAME), ?PRIMECACHEDIR ]),
    Tbls = openfiles( Dir, NumFiles ),

    % Calculate and cache the first slot of prime numbers.
    BasePrimes = ?MODULE:primes( fun(N, _Acc) -> N =< ?DETS_SLOT_BUCKET end ),
    {0, BaseTbl} = proplists:lookup( 0, Tbls ),
    dets:insert( BaseTbl, {?DETS_SLOT_BUCKET, BasePrimes} ),
    dets:sync( BaseTbl ),
    {ok, #gpstate{detsdir=Dir, tbls=Tbls, nfiles=NumFiles}}.


%%-- Calls

handle_call(getstate, _From, State) ->
    {reply, State, State};

handle_call(housekeep, _From, State) ->
    NewState = repair_tables( State ),
    {reply, NewState, NewState};

handle_call({primeswithin, Start, End}, {_Pid, Tag}=From, State) ->
    Keys = range2keys( Start, End ),
    {RcvN, PrimeD} = fetch_slots( Keys, State, From, 0, [] ),
    {reply, {Tag, PrimeD, RcvN}, State};

handle_call({nprimes, N}, {_Pid, Tag}=From, State) ->
    Keys = range2keys( 1, N *20 ),
    {RcvN, PrimeD} = fetch_slots( Keys, State, From, 0, [] ),
    {reply, {Tag, PrimeD, RcvN}, State};

handle_call({isprime, N}, {_Pid, Tag}=From, State) ->
    spawn( ?MODULE, checkprime, [N, From] ),
    {reply, Tag, State};

handle_call({storeprimes, Key, Primes}, _From, State) ->
    Suffix = (Key-1) div ?DETS_FILE_BUCKET,
    erlang:display({ storeprimes, Suffix, Key, length(Primes) }),
    {Suffix, Tbl} = proplists:lookup( Suffix, State#gpstate.tbls ),
    {reply, dets:insert( Tbl, {Key, Primes} ), State}.


%%-- Casts

handle_cast(sync, State) ->
    sync_tables( State );

handle_cast(_, _State) ->
    erlang:error("Unknown cast call").


%%-- Infos

handle_info( timeout, _state ) ->
    ok;

handle_info(_, _State) ->
    erlang:error("Unknown info call").


terminate(normal, State) -> closetables( State ), ok;
terminate(shutdown, State) -> closetables( State ), ok; % Supervisor shutdown
terminate({shutdown, _Reason}, State) -> closetables( State ), ok;
terminate(_Reason, State) -> closetables( State ), ok.  % Probably an error


code_change(_A, _B, _C) ->
    erlang:error("Code change function not implemented.").


%%---- Internal functions

% Prime number proc-reference
server_name() ->
    {ok, Spec} = application:get_env(?APPNAME, childspecs),
    {_M, _F, [A | _As]} = element(2, proplists:lookup(prime, Spec)),
    A.

% Open dets file `FName` using standard Options.
openfile(FName) ->
    %Opts = [ {auto_save, ?DETS_AUTOSAVE}, {max_no_slots, ?DETS_MAX_SLOTS},
    %         {min_no_slots, ?DETS_MIN_SLOTS} ],
    Opts = [ {auto_save, ?DETS_AUTOSAVE} ],
    openfile(FName, Opts).

% Open dets file `FName` using supplied Options `Opts`.
openfile(FName, Opts) -> 
    {ok, Tbl} = dets:open_file(FName, Opts),
    Tbl.

% Open all dets files under path `Dir`, using standard options.
openfiles(Dir, NumFiles) ->
    Fn = fun(I) -> FName = primepath(Dir, I), {I, openfile(FName)} end,
    lists:map( Fn, lists:seq(0, NumFiles-1) ).

closetables(State) ->
    [ dets:close(Tbl) || {_, Tbl} <- State#gpstate.tbls ],
    State#gpstate{tbls=[]}.

primefile(Suffix) ->
    "prime" ++ integer_to_list(Suffix) ++ ".dets".

primepath(Dir,Suffix) ->
    filename:join([ Dir, primefile(Suffix) ]).

range2keys(From, Till) when (From =< Till) and (From > 0) -> 
    lists:seq( ceilKey(From), ceilKey(Till), ?DETS_SLOT_BUCKET ).

ceilKey(Key) when (Key rem ?DETS_SLOT_BUCKET) =:= 0 -> Key;
ceilKey(Key) -> ((Key div ?DETS_SLOT_BUCKET) + 1) * ?DETS_SLOT_BUCKET.

% Repair DETS tables
repair_tables(State) ->
    Opts = [{repair, force}] ,
    _ = [ { dets:close(Tbl), dets:open_file( dets:info(Tbl, filename), Opts ) }
          || {_, Tbl} <- State#gpstate.tbls ],
    Tbls = openfiles(State#gpstate.detsdir, State#gpstate.nfiles),
    State#gpstate{tbls=Tbls}.

% Sync DETS tables to disk
sync_tables(State) ->
    lists:map( fun({_, Tbl}) -> dets:sync(Tbl) end, State#gpstate.tbls ).


% Prime number access functions
fetch_slots([], _State, _From, RcvN, PrimeD) ->
    {RcvN, PrimeD};

fetch_slots([Key | Keys], State, From, RcvN, PrimeD) ->
    Suffix = (Key-1) div ?DETS_FILE_BUCKET,
    {Suffix, Tbl} = proplists:lookup( Suffix, State#gpstate.tbls ),
    case dets:lookup( Tbl, Key ) of
        [Ps] ->
            fetch_slots( Keys, State, From, RcvN, [Ps | PrimeD] );
        [] ->
            Ns = lists:seq( Key-?DETS_SLOT_BUCKET+1, Key ),
            spawn( ?MODULE, filterprimes, [Ns, Key, From] ),
            fetch_slots( Keys, State, From, RcvN+1, PrimeD )
    end.

rcv(_Tag, PrimeD, 0) ->
    PrimeD;
rcv(Tag, PrimeD, RcvN) ->
    receive {Tag, Ps} -> rcv( Tag, [Ps | PrimeD], RcvN-1 ) end.


% Local Interfaces to filter prime numbers from `Ns`. Since this interface is
% launched as a separate process, it is imperative that `ForKey` and `From`
% tag is supplied to identify the file-slot and the requesting process to send
% the result.
filterprimes(Ns, ForKey, {Pid, Tag}) ->
    erlang:display({filterprimes, ForKey}),

    % A sort is required on the resulting list of prime numbers because
    % `filterprimes` tend to rotate the list of `Ns` as it folds over its
    % divisibility. Sometimes it will be sorted, other times it will be
    % reverse sorted.
    Primes = lists:sort( filterprimes(Ns, ?DETS_SLOT_BUCKET) ),
    ?MODULE:store_primes(ForKey, Primes),
    Pid ! {Tag, {ForKey, Primes}}.

% Use `WithKey` to fetch a bunch of prime numbers and filter `Ns` for prime
% numbers. Move to next next slot of prime numbers (from cached file) and
% repeate the same, until eveything in `Ns` is nothing but primes.
filterprimes(Ns, WithKey) ->
    Primes = primesWithin(WithKey-?DETS_SLOT_BUCKET+1, WithKey),
    Fn = fun(N, Acc) -> 
            case num:divisible(N, Primes) of true -> Acc; false -> [N|Acc] end
         end,
    case lists:foldl(Fn, [], Ns) of
        [] -> [];
        NewNs when (WithKey*WithKey) > hd(NewNs) -> lists:reverse(NewNs);
        NewNs -> filterprimes( NewNs, WithKey+?DETS_SLOT_BUCKET )
    end.


% Local Interfaces to check whether `N` is a prime number. Since this
% interface is launched as a separate process, it is imperative that `ForKey`
% and `From` tag is supplied to identify the file-slot and the requesting
% process to send the result.
checkprime(N, {Pid, Tag}) ->
    Pid ! {Tag, checkprime(N, ?DETS_SLOT_BUCKET)};

% Use `WithKey` to compute whether `N` is prime or not. Move to next slot
% of prime numbers (from cached file) and repeate the same, until it is
% conclusively proved that `N` is prime.
checkprime(0, _WithKey) -> false;
checkprime(1, _WithKey) -> false;
checkprime(N, WithKey) ->
    Primes = primesWithin(WithKey-?DETS_SLOT_BUCKET+1, WithKey ),
    PrimeNums = if 
                    N > WithKey -> Primes;
                    true -> lists:takewhile( fun(X) -> X < N end, Primes )
                end,
    case num:divisible( N, PrimeNums ) of
        true -> false;
        false when (WithKey*WithKey) > N -> true;
        false -> checkprime( N, WithKey+?DETS_SLOT_BUCKET )
    end.


% Compute `Count` number of prime numbers, starting from `2`.
primesN( _, 0, Acc ) -> Acc;
primesN( N, Count, Acc ) ->
    SRootN = num:floor( math:sqrt( N )),
    Pred = fun (X) -> X =< SRootN end,
    case num:divisibleWhile( N, Acc, Pred ) of
        true -> primesN( N+1, Count, Acc );
        false -> primesN( N+1, Count-1, Acc ++ [N] )
    end.

% Compute a sequential list of prime numbers, starting from `2`, as long as 
% predicate `Pred` returns true.
primesPred( N, Pred, Acc ) ->
    case Pred(N, Acc) of
        true -> 
            SRootN = num:floor( math:sqrt( N )),
            Pred1 = fun (X) -> X =< SRootN end,
            case num:divisibleWhile(N, Acc, Pred1) of
                false -> primesPred( N+1, Pred, Acc ++ [N] );
                true -> primesPred( N+1, Pred, Acc )
            end;
        false ->
            Acc
    end.


%%---- Local test cases.

-ifdef(EUNIT).

range2keys_test_() ->
    Ref1 = [?DETS_SLOT_BUCKET], 
    Ref2 = lists:seq(?DETS_SLOT_BUCKET, 2000, ?DETS_SLOT_BUCKET),
    [ ?_assertEqual( Ref1, range2keys(1, 10) ),
      ?_assertEqual( Ref2, range2keys(1, 1001) ) ].

ceilKey_test_() ->
    [ ?_assertEqual( 1000, ceilKey(1000) ),
      ?_assertEqual( 1000, ceilKey(1) ),
      ?_assertEqual( 2000, ceilKey(1001) ) ].

primesPred_test_() ->
    Pred = fun(N, _Acc) -> N < 10 end,
    [ ?_assertEqual( [2,3,5,7], primesPred(2, Pred, []) ) ].

primesN_test_() ->
    [ ?_assertEqual( [2,3,5,7,11,13,17,19,23,29], primesN(2, 10, []) ) ].

-endif.
