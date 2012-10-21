-module(varr).
-export([start/0]).
-export([get_config_value/2, get_env/1]).

start() ->
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(eredis),
    ok = application:start(hottub),
    ok = application:start(varr_app).
 
get_config_value(Key, Default) -> %TODO: move get_config_value/2 to get_env/2
    case application:get_env(varr, Key) of
        {ok, Value} -> Value;
        _ -> Default
    end.

get_env(Key) ->
    case application:get_env(varr, Key) of
        {ok, Value} -> Value;
        _ -> undefined
    end.