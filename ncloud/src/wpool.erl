-module(wpool).
-behaviour(gen_server).

-plugin([iprocesspool]).

%% gen_server behaviour.
-export([ handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3 ]).

%% plugin behaviour
-export([ initialize/1 ]).

-include_lib("ncloud/include/plugin.hrl").


%%---- Callbacks for plugin

initialize( _Args ) ->
    {ok, {}}.

default_settings() ->


%%---- Callbacks for gen_server behaviour

handle_call(Req, _From, State) ->
    io:format( "Unknown call, ~w ~w ~n", [Req, State] ).


handle_cast(Req, State) ->
    io:format( "Unknown cast, ~w ~w ~n", [Req, State] ).


handle_info(Req, State) ->
    io:format( "Unknown info request, ~w ~w ~n", [Req, State] ).


terminate(normal, _State) -> ok;
terminate(shutdown, _State) -> ok;
terminate({shutdown, _Reason}, _State) -> ok;
terminate(_Reason, _State) -> ok.


code_change(_A, _B, _C) ->
    erlang:error("Code change function not implemented.").


