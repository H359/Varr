-module(var).
-export([start/0]).

start() ->
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(var_app).