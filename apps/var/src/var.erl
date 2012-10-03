-module(var).
-export([start/0]).
-export([get_config_value/2]).

start() ->
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(eredis),
    ok = application:start(hottub),
    ok = application:start(var_app).

get_config_value(Key, Default) ->
    case application:get_env(var, Key) of
        {ok, Value} -> Value;
        _ -> Default
    end.