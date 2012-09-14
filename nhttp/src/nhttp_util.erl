-module(nhttp_util).

-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "nhttp/include/nhttp.hrl" ).

% @doc: Parse #rule structure into a list of `elements`.
% #rule
%   A construct "#" is defined, similar to "*", for defining lists of
%   elements. The full form is "<n>#<m>element" indicating at least
%   <n> and at most <m> elements, each separated by one or more commas
%   (",") and OPTIONAL linear white space (LWS). This makes the usual
%   form of lists very easy; a rule such as
parse_hashrule(Data) when is_list(Data) ->
    lists:map( fun string:strip/1, string:tokens(Data, ",")).


parse_params(Param) ->
    case re:run( Param, ";([^=]+)(=([^;]+))", [global,{capture,all,list}] ) of
        {match, Ps} ->
            parse_params(Ps);
        _ ->
            ?EMSG( "Invalid params field ~p~n", [Param] ),
            []
    end.

parse_params([], Acc) -> lists:reverse(Acc);
parse_params([[_,K,_,V] | Ps], Acc) ->
    parse_params( Ps, [{string:strip(K), string:strip(V)} | Acc ] ).


%% @doc Convert an iolist to a hexadecimal string.
to_hex(0) -> "0";
to_hex(I) when is_integer(I), I > 0 -> to_hex(I, []);
to_hex(B) -> to_hex(iolist_to_binary(B), []).

to_hex(<<>>, Acc) -> lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]);
to_hex(0, Acc) -> Acc;
to_hex(I, Acc) -> to_hex(I bsr 4, [hexdigit(I band 16#F) | Acc]).

%% @doc Convert an integer less than 16 to a hex digit.
hexdigit(C) when C >= 0, C =< 9 -> C + $0;
hexdigit(C) when C =< 15        -> C + $a - 10.

%% @doc Convert a hex digit to its integer value.
dehex(C) when C >= $0, C =< $9 -> C - $0;
dehex(C) when C >= $a, C =< $f -> C - $a + 10;
dehex(C) when C >= $A, C =< $F -> C - $A + 10.

%% @doc Convert a hexadecimal string to a binary.
to_bin(L) -> to_bin(L, []).

to_bin([], Acc) -> iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
    to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).

%% @doc Convert a hexadecimal string to an integer.
to_int(L) -> erlang:list_to_integer(L, 16).
