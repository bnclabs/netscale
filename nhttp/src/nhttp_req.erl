-module(nhttp_req).

-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "nhttp/include/nhttp.hrl" ).

-export([ parse_req/2 ]).

parse_req(_State, Data) ->
    erlang:display(Data).

parse_req(State, closed) ->
    erlang:display(State).
