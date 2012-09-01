-module(nhttp_app).
-behaviour(supervisor).

% App behaviour
-export([ start/2, prep_stop/1, stop/1, config_change/3 ]).
% Supervisor behaviour
-export([ init/1 ]).

-define( MAXR, 10 ).    % Number of restarts,
-define( MAXT, 5 ).     % in number of seconds.


%%---- Callbacks for application behaviour

start( normal, StartArgs ) ->
    {ok, SupName} = application:get_env( supname ),
    supervisor:start_link({global, SupName}, ?MODULE, StartArgs);

start( {takeover, _Node}, _StartArgs ) ->
    {error, "Does not handle application `takeover`" };

start( {failover, _Node}, _StartArgs ) ->
    {error, "Does not handle application `failover`" }.


%%-- Prepare to stop. Called back before calling back stop/1.
prep_stop( State ) ->
    State.

%%-- Stop this application.
stop( _State ) ->
    ok.


config_change( _Changed, _New, _Old ) ->
    error_logger:error_msg(
        "This application is not yet ready for dynamic code change."),
    ok.


%% Callbacks for supervisor behaviour

init( _Args ) ->
    {ok, ChildSpecs} = application:get_env( nhttp_sup_childspecs ),
    {ok, { {one_for_one, ?MAXR, ?MAXT}, ChildSpecs }}.

