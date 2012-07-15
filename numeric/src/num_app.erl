-module(num_app).
-behaviour(supervisor).

% App behaviour
-export([ start/2, prep_stop/1, stop/1, config_change/3 ]).
% Supervisor behaviour
-export([ init/1 ]).

-define( MAXR, 10 ).    % Number of restarts,
-define( MAXT, 5 ).     % in number of seconds.

%% Callbacks for application behaviour

start( normal, StartArgs ) ->
    {ok, SupName} = application:get_env( supname ),
    supervisor:start_link({global, SupName}, ?MODULE, StartArgs);

start( {takeover, _Node}, _StartArgs ) ->
    erlang:error( "Does not handle application `takeover`" );

start( {failover, _Node}, _StartArgs ) ->
    erlang:error( "Does not handle application `failover`" ).


prep_stop( State ) ->
    State.


stop( _State ) ->
    ok.


config_change( _Changed, _New, _Old ) ->
    erlang:error("This application is not yet ready for dynamic code change.").


%% Callbacks for supervisor behaviour

init( _Args ) ->
    {ok, ChildSpecs} = application:get_env( childspecs ),
    {ok, { {one_for_one, ?MAXR, ?MAXT}, ChildSpecs }}.
