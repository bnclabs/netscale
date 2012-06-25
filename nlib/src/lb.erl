-module(lb). % Load Balancing algorithms

-export([ spawn/1, spawn/3, spawn_link/1, spawn_link/3, spawn_monitor/1,
          spawn_monitor/3, spawn_opt/2, spawn_opt/4, pmap/2 ]).

spawn(Fun) -> {Fun}.
spawn(Module, Function, Args) -> {Module, Function, Args}.
spawn_link(Fun) -> {Fun}.
spawn_link(Module, Function, Args) -> {Module, Function, Args}.
spawn_monitor(Fun) -> {Fun}.
spawn_monitor(Module, Function, Args) -> {Module, Function, Args}.
spawn_opt(Fun, Opts) -> {Fun, Opts}.
spawn_opt(Module, Function, Args, Opts) -> {Module, Function, Args, Opts}.

pmap(Fun, Ls) -> {Fun, Ls}.
