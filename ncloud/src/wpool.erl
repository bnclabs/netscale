-module(wpool).
-behaviour(gen_server).
-behaviour(plugin).
-implements([iprocesspool]).

%% gen_server behaviour.
-export([ handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3 ]).

-include_lib("ncloud/include/plugin.hrl").


%%---- Callbacks for gen_server behaviour

initialize( Args ) ->
    {ok, {}}.


handle_call(Atom, _From, State) ->
    {reply, State, State};
