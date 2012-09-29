-module(var).
-export([start/0]).

start() ->
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(eredis), %dunno if this is actually needed
    ok = application:start(var_app).

get_config_value(Key, Default) ->
    case application:get_env(var, Key) of
        {ok, Value} -> Value;
        _ -> Default
    end.