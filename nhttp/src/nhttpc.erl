-module(nhttpc).

-export([ method_get/3, method_get/1, receive_data/2 ]).

method_get(Host, Port, Options) ->
    {ok, Socket} = gen_tcp:connect( Host, Port, Options ),
    ok = gen_tcp:send( Socket, "GET / HTTP/1.0\r\n\r\n" ),
    receive_data( Socket, [] ).

method_get(Host) -> 
    Options = [binary, {packet, 0}],
    method_get(Host, 80, Options).

receive_data( Socket, SoFar ) ->
    receive
        {tcp,Socket,Bin} -> receive_data(Socket, [Bin|SoFar]);
        {tcp_closed,Socket} -> list_to_binary( lists:reverse(SoFar))
    end.
