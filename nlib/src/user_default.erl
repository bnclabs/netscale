-module(user_default).
-compile(export_all).

% Collect node details on different hosts from .hosts.erlang file and ping
% them.
connectfarms() ->
    io:format("Connecting to nodes ... "),
    net_adm:ping_list( net_adm:host_file() ),
    io:format( "~p ~n", [net_adm:names()] ).

mem() -> erlang:memory().
